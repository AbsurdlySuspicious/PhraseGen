package patnmgen

import net.sf.extjwnl.data.{IndexWord, POS}
import net.sf.extjwnl.dictionary.Dictionary

import scala.collection.JavaConverters._
import patnmgen.Utils._

import scala.util.Random
import scala.util.matching.Regex

object PS {
  val fs: PartialFunction[String, PS] = {
    case "noun"              => Noun
    case "verb"              => Verb
    case "adverb"            => Adverb
    case "adj" | "adjective" => Adj
  }

  private val fsl = fs.lift

  def fromString(s: String): Option[PS] = fsl(s)

  def fromStringT(s: String): TraversableOnce[PS] = fromString(s)

  def fromOrig(ps: POS): PS = ps match {
    case POS.ADJECTIVE => Adj
    case POS.ADVERB    => Adverb
    case POS.VERB      => Verb
    case POS.NOUN      => Noun
    case _             => throw new Exception("invalid POS")
  }
}

sealed class PS(val orig: POS)

case object Noun extends PS(POS.NOUN)
case object Verb extends PS(POS.VERB)
case object Adverb extends PS(POS.ADVERB)
case object Adj extends PS(POS.ADJECTIVE)

sealed trait WordMode

case object Full extends WordMode
case class WithSep(sep: String) extends WordMode
case object RandomWord extends WordMode
case object FirstWord extends WordMode
case object LastWord extends WordMode

case class GeneratorToken(ps: PS, wm: WordMode)
case class GeneratorPattern(p: ParsedPattern, tokens: List[GeneratorToken])

class GeneratorException(m: String) extends Exception(m)

class Generator(dictPath: String) {
  val rnd = new Random
  val dict = Dictionary.getFileBackedInstance(dictPath)

  val sepPref = "sep"
  val sepRe = new Regex(s"""$sepPref\[(.*)\]""")

  def rand(max: Int) = rnd.nextInt(max)

  def ex(msg: String) = throw new GeneratorException(msg)

  def parsePattern(pat: String): GeneratorPattern = {
    val p = patternParse(pat, ("<", ">"))
    val tokens = p.tokens
      .map {
        case (_, s) =>
          val spl = s.split(' ').toList
          val (psS, wm) = spl match {
            case psI :: Nil => (psI, Full)
            case wmI :: psI :: Nil =>
              val wmM = wmI match {
                case "src" => Full
                case "fw"  => FirstWord
                case "lw"  => LastWord
                case "rw"  => RandomWord
                case sp if sp.startsWith(sepPref) =>
                  val m =
                    sepRe.findFirstMatchIn(sp).getOrElse(ex("invalid separator wm"))
                  WithSep(m.group(1))
                case x => ex(s"invalid wm: $x")
              }
              (psI, wmM)
            case x => ex(s"invalid token: ${x.mkString(" ")}")
          }

          val ps = PS.fromString(psS).getOrElse(ex(s"invalid ps: $psS"))
          GeneratorToken(ps, wm)
      }

    GeneratorPattern(p, tokens)
  }

  def randomIdxWord(pos: PS) =
    dict.getRandomIndexWord(pos.orig)

  def getIdxWordsForPattern(
      p: GeneratorPattern): List[(GeneratorToken, IndexWord)] =
    p.tokens.map(t => t -> randomIdxWord(t.ps))

  def synSelector(w: IndexWord, wm: WordMode): String = {
    val senses = w.getSenses.asScala.toList
    val syn = senses.head
    val words = syn.getWords.asScala
    val word = words(rand(words.length))
    val lemma = word.getLemma
    def split = lemma.split(' ')

    println(word)

    wm match {
      case Full         => lemma
      case FirstWord    => split.head
      case LastWord     => split.last
      case WithSep(sep) => split.mkString(sep)
      case RandomWord =>
        val s = split
        s(rand(s.length))
    }
  }

}
