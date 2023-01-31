package lore
import munit.FunSuite
import lore.AST._
import io.circe.parser.decode
import cats.parse
import cats.implicits._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import cats.data.NonEmptyList

class WholeProgramParsing extends FunSuite:
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
    Parser.prog.parseAll(prog) match
      case Left(e)  => fail(e.show)
      case Right(e) => ()
  }

  test("calendar new") {
    val prog = readProg(Path.of("examples/calendar_new.lore"))
    val astStr = readProg(Path.of("examples/calendar_new.ast"))
    Parser.prog.parseAll(prog) match
      case Left(e) => fail(e.show) // parsing failure
      case Right(e) =>
        decode[NonEmptyList[Term]](astStr) match
          // check if AST matches expectation
          case Right(ast) => assertEquals(e, ast)
          case Left(err)  => fail(err.show)
  }
