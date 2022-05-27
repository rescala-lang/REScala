package lore
import minitest._
import lore.AST._
import cats.parse
import cats.implicits._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object WholeProgramParsing extends SimpleTestSuite:
  def assertParses[A](p: parse.Parser[A], expr: String): Unit =
    p.parseAll(expr) match {
      case Right(_) => ()
      case Left(x)  => fail(x.toString)
    }

  def readProg(path: Path): String =
    String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  test("calendar") {
      val prog = readProg(Path.of("examples/calendar.fr"))
      assertParses(Parser.prog, prog)
  }
