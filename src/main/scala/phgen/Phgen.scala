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

import org.jline.builtins.Completers
import org.jline.reader.{Expander, LineReader, LineReaderBuilder, UserInterruptException}
import phgen.Utils._
import scopt.OParser

import scala.io.StdIn

case class Opts(
    dict: Option[String] = None,
    count: Int = 10,
    countSyn: Int = 1,
    patMode: PatMode = PatRandom,
    pat: List[String] = Nil,
    interactive: Boolean = false,
    senses: Boolean = false
)

object PatMode {
  val fromString: PartialFunction[String, PatMode] = {
    case "round" | "round-robin" => PatRoundRobin
    case "random"                => PatRandom
  }

  def validate(s: String): Boolean =
    fromString.isDefinedAt(s)
}

sealed trait PatMode
case object PatRoundRobin extends PatMode
case object PatRandom extends PatMode

object Phgen extends App {
  val ts = time
  val version = "1.0"

  val dop = Opts()

  def esc(msg: String, code: Int = 1): Nothing = {
    println(msg)
    System.exit(code)
    throw new Exception
  }

  val psts = time
  val optB = OParser.builder[Opts]
  val optP = {
    import optB._

    OParser.sequence(
      programName("phgen"),
      head("PhraseGen", this.version),
      arg[String]("PATTERNS")
        .unbounded()
        .optional()
        .text("Patterns that will be used for generation")
        .action((p, o) => o.copy(pat = o.pat :+ p)),
      opt[Unit]('I', "interactive")
        .text("Interactive mode (passed patterns will be ignored)")
        .action((_, o) => o.copy(interactive = true)),
      opt[String]('d', "dict")
        .text(
          s"Path to WordNet dictionary file (by default embedded wn3.1 will be used)")
        .action((f, o) => o.copy(dict = Some(f))),
      opt[Int]('c', "count")
        .text(s"Count of names to generate (default ${dop.count})")
        .action((c, o) => o.copy(count = c)),
      opt[Int]('y', "syncount")
        .text(
          s"Count of random synonyms for each phrase (default ${dop.countSyn})")
        .action((c, o) => o.copy(countSyn = c)),
      opt[String]('m', "pattern-mode")
        .text("Pattern selection mode: round, random (default: random)")
        .action((m, o) => o.copy(patMode = PatMode.fromString(m)))
        .validate { m =>
          if (PatMode.validate(m)) success
          else failure("Invalid pattern mode")
        },
      opt[Unit]('s', "show-senses")
        .text("Show WordNet (alternative) senses for each used word/synset")
        .action((_, o) => o.copy(senses = true)),
      help("help")
        .text("Show this help")
    )
  }
  val pste = time

  val ppts = time
  val opts = OParser.parse(optP, args, dop) match {
    case Some(o) => o
    case None    => esc("")
  }
  val ppte = time

  val gts = time
  val g = new Generator(opts.dict)
  val gte = time
  val synSep = "\n  "

  val te = time

  val times =
    s"""
       |Whole bootstrap:     ${te - ts}ms
       |Opt parser setup:    ${pste - psts}ms
       |Opt parser routine:  ${ppte - ppts}ms
       |Generator setup:     ${gte - gts}ms
     """.stripMargin

  println(times)

  def interactive(): Unit = {
    var count = opts.count
    var syncount = opts.countSyn
    var senses = opts.senses

    val rd = LineReaderBuilder.builder().build()

    while (true) try {
      println("enter pattern (or :help):")
      val input = rd.readLine()

      input.trim.split(' ').toList match {
        case ":help" :: Nil =>
          println("""
            |Usage: PATTERN or command
            |
            |Commands:
            |:exit - exit interactive mode
            |:count, :syncount - show or change corresponding counts
            |:senses - toggle showing senses
          """.stripMargin)
        case ":exit" :: Nil          => return
        case ":count" :: Nil         => println(count)
        case ":count" :: s :: Nil    => count = s.toInt
        case ":syncount" :: Nil      => println(syncount)
        case ":syncount" :: s :: Nil => syncount = s.toInt
        case ":senses" :: Nil =>
          senses = !senses
          println("show senses: " + senses)
        case x :: _ if x.startsWith(":") =>
          println(s"wrong command: $x")
        case "" :: Nil => ()
        case _ =>
          val pat = g.parsePattern(input)
          println()
          for (i <- 1 to count) {
            val (words, rp) = g.randomForPattern(pat, syncount)
            if (senses) printSenses(words)
            println(rp.mkString(synSep))
          }
          println()
      }
    } catch {
      case _: UserInterruptException => return
      case x: Exception => println("error:\n" + x)
    }
  }

  if (opts.interactive) interactive()
  else {
    val synCount = opts.countSyn
    val senses = opts.senses

    val op = opts.pat
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(g.parsePattern)

    if (op.isEmpty)
      esc(
        "No patterns provided.\nEither use interactive mode or provide one or more patterns.\n")

    val pats =
      opts.patMode match {
        case PatRoundRobin => Iterator.continually(op).flatten
        case PatRandom     => new RandomIter(op.toVector)
      }

    for (_ <- 0 until opts.count) {
      val p = pats.next()

      val (words, rp) = g.randomForPattern(p, synCount)
      if (senses) printSenses(words)
      println(rp.mkString(synSep))
    }
  }

}
