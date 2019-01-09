package patnmgen

import org.scalatest.{FlatSpec, Matchers}
import Utils._

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
