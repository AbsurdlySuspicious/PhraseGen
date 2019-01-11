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

import org.scalatest.{FlatSpec, Matchers}
import phgen.Utils._

class UtilTest extends FlatSpec with Matchers {

  behavior of "Pattern parser"

  it should "parse patterns and apply token-replaces" in {

    val mapper: ((Int, String)) => String = {
      case (i,t) => s"|$i$t|"
    }

    val bounds = ("<", ">")

    val inp = List(
      "foo<A>bar<B>baz" -> "foo|0A|bar|1B|baz",
      "<A>foobar" -> "|0A|foobar",
      "<E>-<A>--<C>" -> "|0E|-|1A|--|2C|",
      "foobar<FOO>" -> "foobar|0FOO|",
      "aaa<FOO>bbb<BARR>" -> "aaa|0FOO|bbb|1BARR|"
    )

    inp.foreach {
      case (pat, ref) =>
        println(pat)
        val parsed = patternParse(pat, bounds)
        val mapped = parsed.tokens.map(mapper)
        val resp = makePatternStrList(parsed, mapped)
        resp shouldBe ref
    }

  }

}
