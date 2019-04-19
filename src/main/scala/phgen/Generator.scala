/*
 *    Copyright 2019 AbsurdlySuspicious
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package phgen

import java.io.{File, FileInputStream}

import net.sf.extjwnl.data.{IndexWord, POS}
import net.sf.extjwnl.dictionary.Dictionary
import phgen.Utils._

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

object PS {
  def fromOrig(ps: POS): PS = ps match {
    case POS.ADJECTIVE => Adj
    case POS.ADVERB    => Adverb
    case POS.VERB      => Verb
    case POS.NOUN      => Noun
    case _             => throw new Exception("invalid POS")
  }
}

sealed class PS(val orig: POS, val ext: String)

case object Noun   extends PS(POS.NOUN, "noun")
case object Verb   extends PS(POS.VERB, "verb")
case object Adverb extends PS(POS.ADVERB, "adv")
case object Adj    extends PS(POS.ADJECTIVE, "adj")

sealed trait WordMode

case class WithSep(sep: String) extends WordMode
case object Full                extends WordMode
case object RandomWord          extends WordMode
case object FirstWord           extends WordMode
case object LastWord            extends WordMode

case class GeneratorToken(ps: PS, wm: WordMode, searchQuery: Option[String])
case class GeneratorPattern(p: ParsedPattern, tokens: List[GeneratorToken]) {
  def patStr(t: List[String]): String = makePatternStrList(p, t)
}

class GeneratorException(m: String) extends Exception(m)

trait Generator {

  val bounds = ("[", "]")

  val sepPref = "sep"
  val sepRe   = new Regex(sepPref + """\((.*)\)""")

  val posRePOS   = "pos"
  val posReParam = "param"
  val posRe      = new Regex("""(.+)\((.*)\)""", posRePOS, posReParam)

  protected def ex(msg: String) = throw new GeneratorException(msg)

  def parsePattern(pat: String): GeneratorPattern = {
    val p = patternParse(pat, bounds)
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
                    sepRe
                      .findFirstMatchIn(sp)
                      .getOrElse(ex("invalid separator wm"))
                  WithSep(m.group(1))
                case x => ex(s"invalid wm: $x")
              }
              (psI, wmM)
            case x => ex(s"invalid token: ${x.mkString(" ")}")
          }

          val posMatch = posRe.findFirstMatchIn(psS)
          val (posName, posParam) = posMatch match {
            case Some(m) => m.group(posRePOS) -> Some(m.group(posReParam))
            case None    => (psS, None)
          }

          val ps = posName match {
            case "n" | "noun"        => Noun
            case "v" | "verb"        => Verb
            case "adv" | "adverb"    => Adverb
            case "adj" | "adjective" => Adj
            case x                   => ex(s"invalid ps: $x")
          }

          GeneratorToken(ps, wm, posParam)
      }

    GeneratorPattern(p, tokens)
  }

  def generatorName: String

  def randomForPattern(pat: GeneratorPattern,
                       synCount: Int): (List[IndexWord], List[String])
}

class GeneratorJwnl(dictPath: Option[String]) extends Generator {
  val generatorName = "jwnl"

  val dict = dictPath match {
    case Some(p) => Dictionary.getFileBackedInstance(p)
    case None    => Dictionary.getDefaultResourceInstance
  }

  val rnd = new Random

  protected def rand(max: Int) = rnd.nextInt(max)

  protected def randElem[T](s: Seq[T]): T = s(rand(s.length))

  protected def getIdxWordsForPattern(
      p: GeneratorPattern): List[(GeneratorToken, IndexWord)] =
    p.tokens.map { t =>
      val word = t.searchQuery match {
        case Some(q) => dict.lookupIndexWord(t.ps.orig, q)
        case None    => dict.getRandomIndexWord(t.ps.orig)
      }
      (t, word)
    }

  protected def synSelector(w: IndexWord, wm: WordMode): String = {
    val senses = w.getSenses.asScala
    val syn    = randElem(senses)
    val words  = syn.getWords.asScala
    val word   = randElem(words)
    val lemma  = word.getLemma
    def split  = lemma.split(' ')

    //println(word)

    wm match {
      case Full         => lemma
      case FirstWord    => split.head
      case LastWord     => split.last
      case WithSep(sep) => split.mkString(sep)
      case RandomWord   => randElem(split)
    }
  }

  def randomForPattern(pat: GeneratorPattern,
                       synCount: Int): (List[IndexWord], List[String]) = {
    val newTk = getIdxWordsForPattern(pat)

    val res = for (_ <- 1 to synCount) yield {
      val newRp = for ((t, w) <- newTk) yield synSelector(w, t.wm)
      pat.patStr(newRp)
    }

    (newTk.map(_._2), res.toList)
  }

}

case class NatPosFiles(index: File, data: File)

class GeneratorNative(dictPath: Option[String]) extends Generator {
  val generatorName = "native"

  val dictDir = dictPath match {
    case Some(p) => new File(p)
    case None    => new File(getClass.getResource("/dict-native").getPath)
  }

  val res = (p: String) => new File(dictDir, p)
  val wn  = res.compose[String](p => s"wn3.1/$p")
  val posF = ((p: PS) => p.ext).andThen(e =>
    NatPosFiles(wn(s"index.$e"), wn(s"data.$e")))

  val wnFiles =
    List(Noun, Verb, Adj, Adverb)
      .map(p => p -> posF(p))
      .toMap

  val indexOffsets = {
    val s     = Source.fromFile(res("indexOffsets"))
    val lr    = s.getLines()
    val count = lr.next().toInt
    val o     = new ArrayBuffer[Int](count)
    for ((l, i) <- lr.zipWithIndex) o(i) = l.toInt
    s.close()
    o.toVector
  }

  override def randomForPattern(pat: GeneratorPattern, synCount: Int) = ???

}
