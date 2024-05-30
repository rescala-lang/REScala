package lore
import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import lore.ast.*
import lore.ast.Codecs.given
import munit.FunSuite

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}

class WholeProgramParsing extends FunSuite {
  def readResource(name: String): String =
    String(
      getClass.getClassLoader.getResourceAsStream(name).readAllBytes,
      StandardCharsets.UTF_8
    ).linesIterator.mkString("\n")

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
    Parser.prog.parseAll(prog) match {
      case Left(e)  => fail(e.show)
      case Right(e) => ()
    }
  }

  test("imports") {
    val prog   = "//> viperimport deps/calendar_header.vpr"
    val astStr = readResource("viperimport.ast")
    Parser.prog.parseAll(prog) match {
      case Left(e)       => fail(e.show) // parsing failure
      case Right(parsed) =>
        // uncomment when AST format changes
//         import java.nio.file.{Files, Path}
//         Files.write(
//           Path.of("src/test/resources/viperimport.ast"),
//           writeToString(parsed).getBytes(StandardCharsets.UTF_8)
//         )
        Try(readFromString[NonEmptyList[Term]](astStr)) match {
          // check if AST matches expectation
          case Success(ast) =>
            assertEquals(parsed, ast);
          case Failure(err) => fail(err.toString)
        }
    }
  }

  test("calendar new") {
    val prog   = readResource("calendar_new.lore")
    val astStr = readResource("calendar_new.ast")
    Parser.prog.parseAll(prog) match {
      case Left(e)       => fail(e.show) // parsing failure
      case Right(parsed) =>
        // uncomment when AST format changes
//         import java.nio.file.{Files, Path}
//         Files.write(
//           Path.of("src/test/resources/calendar_new.ast"),
//           writeToString(parsed).getBytes(StandardCharsets.UTF_8)
//         )
        Try(readFromString[NonEmptyList[Term]](astStr)) match {
          // check if AST matches expectation
          case Success(ast) =>
            assertEquals(parsed, ast);
          case Failure(err) => fail(err.toString)
        }
    }
  }

  test("calendar advanced") {
    val prog   = readResource("calendar_advanced.lore")
    val astStr = readResource("calendar_advanced.ast")
    Parser.prog.parseAll(prog) match {
      case Left(e)       => fail(e.show) // parsing failure
      case Right(parsed) =>
        // uncomment when AST format changes
//        import java.nio.file.{Files, Path}
//        Files.write(
//          Path.of("src/test/resources/calendar_advanced.ast"),
//          writeToArray(parsed)
//          //writeToString(parsed).getBytes(StandardCharsets.UTF_8)
//        )
        Try(readFromString[NonEmptyList[Term]](astStr)) match {
          // check if AST matches expectation
          case Success(ast) =>
            assertEquals(parsed, ast);
          case Failure(err) => fail(err.toString)
        }
    }
  }
}
