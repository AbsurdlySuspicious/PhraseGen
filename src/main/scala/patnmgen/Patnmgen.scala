package patnmgen

import scopt.OParser

import scala.util.Random
import Utils._

import scala.io.StdIn

case class Opts(
    dict: String = "",
    count: Int = 10,
    countSyn: Int = 3
)

object Test extends App {

  val g = new Generator("local/dict")

  val pat = StdIn.readLine()
  val gp = g.parsePattern(pat)

}

object Patnmgen extends App {

  val dop = Opts()

  def esc(msg: String, code: Int = 1): Nothing = {
    println(msg)
    System.exit(code)
    throw new Exception("uhh exit")
  }

  val optB = OParser.builder[Opts]
  val optP = {
    import optB._

    OParser.sequence(
      programName("patnmgen"),
      opt[String]('d', "dict")
        .text("Path to WordNet dictionary file")
        .required()
        .action((f, o) => o.copy(dict = f)),
      opt[Int]('c', "count")
        .text(s"Count of names to generate (default ${dop.count})")
        .action((c, o) => o.copy(count = c)),
      opt[Int]("syncount")
        .abbr("sc")
        .text(
          s"Count of random synonyms for each name (default ${dop.countSyn})")
        .action((c, o) => o.copy(countSyn = c)),
      help("help")
    )
  }

  val opts = OParser.parse(optP, args, dop) match {
    case Some(o) => o
    case None    => esc("Can't parse options")
  }

}
