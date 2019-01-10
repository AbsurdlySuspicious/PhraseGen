package patnmgen

import net.sf.extjwnl.data.IndexWord

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

object Utils {

  case class ParsedPattern(
      around: List[String],
      tokens: List[(Int, String)]
  )

  def patternParse(input: String, tokenBounds: (String, String)): ParsedPattern = {

    val (lb, rb) = tokenBounds
    val rbl = rb.length
    val bt = new mutable.ArrayBuffer[String]
    val tk = new mutable.ArrayBuffer[(Int, String)]

    val buf = new StringBuffer(input)
    val inl = input.length
    var idx = 0

    var i = 0
    while (idx < inl) {
      val open = buf.indexOf(lb, idx)

      if (open < 0) {
        val lastPref = buf.substring(idx, inl)
        bt += lastPref
        idx = inl
      } else {
        val close = buf.indexOf(rb, open)
        val pref = buf.substring(idx, open)
        val token = i -> buf.substring(open, close).stripPrefix(lb)

        bt += pref
        tk += token
        idx = close + rbl
        i += 1
      }
    }

    ParsedPattern(bt.toList, tk.toList)
  }

  def makePatternStrList(pattern: ParsedPattern,
                         replaceTokens: List[String]): String = {
    val b = new StringBuffer()
    val rp = mutable.ArrayStack(replaceTokens: _*)
    val ar = mutable.ArrayStack(pattern.around: _*)

    while (ar.nonEmpty) {
      b.append(ar.pop())
      if (rp.nonEmpty) b.append(rp.pop())
    }

    b.toString
  }

  def printSenses(words: List[IndexWord]): Unit = {
    println("\n=== senses: ===")
    for (w <- words) {
      println(s"for word ${w.getLemma}:")
      for (s <- w.getSenses.asScala) println(s)
      println()
    }
    println("===============\n")
  }

  class RandomIter[T](src: Seq[T]) extends Iterator[T] {
    val rnd = new Random
    val max = src.length

    override def hasNext = true
    override def next(): T = src(rnd.nextInt(max))
  }

}
