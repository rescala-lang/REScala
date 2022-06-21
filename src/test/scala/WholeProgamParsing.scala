package lore
import minitest._
import lore.AST._
import cats.parse
import cats.implicits._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object WholeProgramParsing extends SimpleTestSuite:
  def readProg(path: Path): String =
    String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  // test("calendar old") {
  //     val prog = readProg(Path.of("examples/calendar.fr"))
  //     assertParses(Parser.prog, prog)
  // }

  test("simple prog") {
    val prog = """
      |type Calendar = AWSet[Appointment]
      |val work: Source[Calendar] = Source(AWSet())
      |5 + 24 * 10 > 0 ==> true
      |""".stripMargin

    assert(Parser.prog.parseAll(prog).isRight)
  }

  test("calendar new") {
    val prog = readProg(Path.of("examples/calendar_new.lore"))
    val printer = new Prettyprint("calendar_new.lore", prog)
    Parser.prog.parseAll(prog) match
      case Left(e)  => fail(printer.prettyprint(e))
      case Right(e) => ()
  }
