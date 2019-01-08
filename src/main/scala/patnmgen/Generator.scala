package patnmgen

import net.sf.extjwnl.data.POS
import net.sf.extjwnl.dictionary.Dictionary

sealed class PS(val orig: POS)

case object Noun extends PS(POS.NOUN)
case object Verb extends PS(POS.VERB)
case object Adverb extends PS(POS.ADVERB)
case object Adj extends PS(POS.ADJECTIVE)

case class SynSet(syns: List[String])

class Generator(dictPath: String) {
  val dict = Dictionary.getFileBackedInstance(dictPath)

  def randomIdxWord(pos: PS) =
    dict.getRandomIndexWord(pos.orig)



}
