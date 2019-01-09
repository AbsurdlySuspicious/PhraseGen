package patnmgen

import scopt.OParser

import scala.util.Random
import Utils._

import scala.io.StdIn

case class Opts(
    dict: String = "local/dict",
    count: Int = 10,
    countSyn: Int = 1,
    interact: Boolean = false
)

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
      opt[Unit]('I', "interactive")
        .text("Interactive mode (passed patterns will be ignored)")
        .action((_, o) => o.copy(interact = true)),
      opt[String]('d', "dict")
        .text(s"Path to WordNet dictionary file (default ${dop.dict})")
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
    case None    => esc("")
  }

  val g = new Generator(opts.dict)

  def interactive(): Unit = {
    var count = opts.count
    var syncount = opts.countSyn
    while (true) try {
      println("enter pattern (or :help):")
      val input = StdIn.readLine()

      input.trim.split(' ').toList match {
        case ":help" :: Nil =>
          println("""
            |Usage: PATTERN or command
            |
            |Commands:
            |:exit - exit interactive mode
            |:count, :syncount - show or change corresponding counts
          """.stripMargin)
        case ":exit" :: Nil          => return
        case ":count" :: Nil         => println(count)
        case ":count" :: s :: Nil    => count = s.toInt
        case ":syncount" :: Nil      => println(syncount)
        case ":syncount" :: s :: Nil => syncount = s.toInt
        case _ =>
          val pat = g.parsePattern(input)
          println()
          for (i <- 1 to count) {
            val rp = g.randomForPattern(pat, syncount)
            //println(s"===== $i =====\n${rp.mkString("\n")}\n")
            println(rp.mkString("; "))
          }
          println()
      }
    } catch {
      case x: Exception => println("error:\n" + x)
    }
  }

  if (opts.interact) interactive()
  else {}

}
