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

import java.io.{
  BufferedInputStream,
  BufferedReader,
  File,
  FileInputStream,
  FileReader,
  RandomAccessFile
}

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

case class GeneratorResponse(syn: List[String], senses: List[String])
case class GeneratorToken(ps: PS, wm: WordMode, searchQuery: Option[String])
case class GeneratorPattern(around: List[String],
                            tokens: List[GeneratorToken]) {
  def patStr(repTokens: List[String]): String =
    makePatternStrList(around, repTokens)
}

class GeneratorException(m: String) extends Exception(m)

trait Generator {

  val bounds = ("[", "]")

  val sepPref = "sep"
  val sepRe   = new Regex(sepPref + """\((.*)\)""")

  val posRePOS   = "pos"
  val posReParam = "param"
  val posRe      = new Regex("""(.+)\((.*)\)""", posRePOS, posReParam)

  val rnd = new Random

  protected def rand(max: Int) = rnd.nextInt(max)

  protected def randElem[T](s: Seq[T]): T = s(rand(s.length))

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

    GeneratorPattern(p.around, tokens)
  }

  def applyWM(lemma: String, wm: WordMode): String = {
    def split = lemma.split(' ')

    wm match {
      case Full         => lemma
      case FirstWord    => split.head
      case LastWord     => split.last
      case WithSep(sep) => split.mkString(sep)
      case RandomWord   => randElem(split)
    }
  }

  def generatorName: String

  def cleanup(): Unit

  def randomForPattern(pat: GeneratorPattern,
                       synCount: Int,
                       needSenses: Boolean): GeneratorResponse
}

class GeneratorJwnl(dictPath: Option[String]) extends Generator {
  val generatorName = "jwnl"

  def cleanup(): Unit = ()

  val dict = dictPath match {
    case Some(p) => Dictionary.getFileBackedInstance(p)
    case None    => Dictionary.getDefaultResourceInstance
  }

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
    applyWM(lemma, wm)
  }

  def getSenses(words: List[IndexWord]): List[String] =
    words.flatMap(_.getSenses.asScala.map(_.toString))

  def randomForPattern(pat: GeneratorPattern,
                       synCount: Int,
                       needSenses: Boolean): GeneratorResponse = {
    val newTk = getIdxWordsForPattern(pat)

    val res = for (_ <- 1 to synCount) yield {
      val newRp = for ((t, w) <- newTk) yield synSelector(w, t.wm)
      pat.patStr(newRp)
    }

    val s =
      if (needSenses) getSenses(newTk.map(_._2))
      else Nil
    GeneratorResponse(res.toList, s)
  }

}

trait NatFileHolder {
  def rafRO(f: File) = new RandomAccessFile(f, "r")
  def close(): Unit
}

case class NatPosFiles(index: File, data: File, offsets: File)
    extends NatFileHolder {

  val indexR = rafRO(index)
  val dataR  = rafRO(data)

  def close(): Unit = {
    indexR.close()
    dataR.close()
  }

  val indexOffsets: Array[Int] =
    if (offsets.exists) {
      val s     = Source.fromFile(offsets)
      val lr    = s.getLines()
      val count = lr.next().toInt
      val o     = new ArrayBuffer[Int](count)
      for ((l, i) <- lr.zipWithIndex) o(i) = l.toInt
      s.close()
      o.toArray
    }
    else {
      val lf = '\n'.toInt
      val o  = new ArrayBuffer[Int]
      val s = new BufferedInputStream(
        new FileInputStream(index)
      )
      var off = 0
      var b   = s.read()
      while (b >= 0) {
        off += 1
        if (b == lf) o += off
        b = s.read()
      }
      s.close()
      o.toArray
    }

  val indexOffsetsLength =
    indexOffsets.length

}

case class NatIndex(pos: PS, lemma: String, sensesOff: List[Int])

case class NatData(pos: PS, words: List[String], sense: String)

object GeneratorNative {
  val numPat = "[0-9]+".r.pattern
  val isNum  = (s: String) => numPat.matcher(s).matches
  val notNum = isNum.andThen(!_)
}

class GeneratorNative(dictPath: Option[String]) extends Generator {
  import GeneratorNative._
  val generatorName = "native"

  val dictDir = dictPath match {
    case Some(p) => new File(p)
    case None    => new File(getClass.getResource("/dict-native/wn3.1").getPath)
  }

  val res = (p: String) => new File(dictDir, p)
  val wn  = res.compose[String](p => s"$p/")
  val posF = ((p: PS) => p.ext).andThen(e =>
    NatPosFiles(wn(s"index.$e"), wn(s"data.$e"), wn(s"indexOffsets.$e")))

  val wnFiles: Map[PS, NatPosFiles] =
    List(Noun, Verb, Adj, Adverb)
      .map(p => p -> posF(p))
      .toMap

  def cleanup(): Unit = {
    wnFiles.values.foreach(_.close())
  }

  def line(pos: PS, off: Int, f: NatPosFiles => RandomAccessFile): String = {
    val raf = f(wnFiles(pos))
    raf.seek(off)
    raf.readLine()
  }

  def indexLine(pos: PS, off: Int): NatIndex = {
    // lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt  synset_offset  [synset_offset...]
    val rawLemma :: posLetter :: _ :: _ :: tailPSym =
      line(pos, off, _.indexR).split(" ").toList

    val _ :: _ :: rawSenses =
      tailPSym.dropWhile(notNum)

    val lemma  = rawLemma.replaceAll("_", " ")
    val senses = rawSenses.map(_.toInt)
    NatIndex(pos, lemma, senses)
  }

  def dataLine(pos: PS, off: Int): NatData = {
    // synset_offset  lex_filenum  ss_type  w_cnt  word  lex_id  [word  lex_id...]  p_cnt  [ptr...]  [frames...] | gloss
    val data :: gloss =
      line(pos, off, _.dataR).split("|").toList

    val dataOff :: _ :: _ :: _ :: tailWords =
      data.split(" ").toList

    val words = tailWords
      .grouped(2)
      .collect {
        case word :: lex :: Nil if isNum(lex) => word
      }
      .toList

    val sense =
      gloss.headOption.map(_.trim).getOrElse("")

    NatData(pos, words, sense)
  }

  def randomOffset(pos: PS): Int = {
    val f = wnFiles(pos)
    f.indexOffsets(rand(f.indexOffsetsLength))
  }

  override def randomForPattern(pat: GeneratorPattern,
                                synCount: Int,
                                needSenses: Boolean): GeneratorResponse = {
    val (repTokens, senses) = pat.tokens
      .map {
        case GeneratorToken(pos, wm, _) => // todo search
          val o = randomOffset(pos)
          val i = indexLine(pos, o)
          val s =
            if (needSenses)
              i.sensesOff
                .map(so => dataLine(pos, so))
                .map(d => s"| [${d.pos.ext}] ${d.words.mkString(", ")}: ${d.sense}")
            else Nil
          val word = applyWM(i.lemma, wm)
          (word, s)
      }
      .foldRight(
        (List.empty[String], List.empty[String])
      ) {
        case ((w, s), (ws, ss)) => (w :: ws, s ::: ss)
      }

    val singleSyn = pat.patStr(repTokens) // todo syns (or rm)
    GeneratorResponse(singleSyn :: Nil, senses)
  }

}
