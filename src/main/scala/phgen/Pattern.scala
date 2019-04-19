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

import scala.collection.mutable

object Pattern {
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

  def makePatternStrList(around: List[String],
                         replaceTokens: List[String]): String = {
    val b = new StringBuffer()
    val rp = mutable.ArrayStack(replaceTokens: _*)
    val ar = mutable.ArrayStack(around: _*)

    while (ar.nonEmpty) {
      b.append(ar.pop())
      if (rp.nonEmpty) b.append(rp.pop())
    }

    b.toString
  }
}
