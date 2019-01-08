import java.nio.CharBuffer

import scala.collection.mutable

object Utils {

  case class ParsedPattern[T](
      around: List[String],
      tokens: List[T]
  )

  def patternParse[T](input: String,
                      tokenMap: Map[String, T],
                      tokenBounds: (String, String),
                      unknownToken: T): ParsedPattern[T] = {

    val (lb, rb) = tokenBounds
    val rbl = rb.length
    val bt = new mutable.ArrayStack[String]
    val tk = new mutable.ArrayStack[T]

    val buf = new StringBuffer(input)
    val inl = input.length
    var idx = 0

    while (idx < inl) {
      val open = buf.indexOf(lb, idx)

      if (open < 0) {
        val lastPref = buf.substring(idx, inl)
        bt.push(lastPref)
        idx = inl
      }
      else {
        val close = buf.indexOf(rb, open)
        val pref = buf.substring(idx, open)
        val tokenStr = buf.substring(open, close).stripPrefix(lb)
        val token = tokenMap.getOrElse(tokenStr, unknownToken)

        bt.push(pref)
        tk.push(token)
        idx += (close + rbl)
      }
    }

    ParsedPattern(bt.toList, tk.toList)
  }

  def makePatternStr[T](pattern: ParsedPattern[T], replaceTokens: List[String]): String = {
    val b = new StringBuffer()
    val rp = mutable.ArrayStack(replaceTokens:_*)
    val ar = mutable.ArrayStack(pattern.around:_*)

    while (ar.nonEmpty) {
      b.append(ar.pop())
      if (rp.nonEmpty) b.append(rp.pop())
    }

    b.toString
  }

}
