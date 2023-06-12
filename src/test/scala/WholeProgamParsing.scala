package lore
import munit.FunSuite
import lore.AST._
import io.circe.parser.decode
import cats.parse
import cats.implicits._
import java.nio.charset.StandardCharsets
// import io.circe.syntax.*
// import java.nio.file.{Files, Path}
import cats.data.NonEmptyList

class WholeProgramParsing extends FunSuite:
  def readResource(name: String): String =
    String(
      getClass.getClassLoader.getResourceAsStream(name).readAllBytes,
      StandardCharsets.UTF_8
    )

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

  test("imports"):
    val prog = """
      |//> viperimport deps/calendar_header.vpr
      |val a: Bool = 10 + 5 ==> true
      |""".stripMargin
    val astStr = readResource("viperimport.ast")
    Parser.prog.parseAll(prog) match
      case Left(e)       => fail(e.show) // parsing failure
      case Right(parsed) =>
        // uncomment when AST format changes
        // Files.write(
        //   Path.of("src/test/resources/viperimport.ast"),
        //   parsed.asJson.toString.getBytes(StandardCharsets.UTF_8)
        // )
        decode[NonEmptyList[Term]](astStr) match
          // check if AST matches expectation
          case Right(ast) =>
            assertEquals(parsed, ast);
          case Left(err) => fail(err.show)

  test("calendar new") {
    val prog = readResource("calendar_new.lore")
    val astStr = readResource("calendar_new.ast")
    Parser.prog.parseAll(prog) match
      case Left(e)       => fail(e.show) // parsing failure
      case Right(parsed) =>
        // uncomment when AST format changes
        // Files.write(
        //   Path.of("examples/calendar_new.ast"),
        //   parsed.asJson.toString.getBytes(StandardCharsets.UTF_8)
        // )
        decode[NonEmptyList[Term]](astStr) match
          // check if AST matches expectation
          case Right(ast) =>
            assertEquals(parsed, ast);

          case Left(err) => fail(err.show)
  }

  test("calendar advanced") {
    val prog = readResource("calendar_advanced.lore")
    val astStr = readResource("calendar_advanced.ast")
    Parser.prog.parseAll(prog) match
      case Left(e)       => fail(e.show) // parsing failure
      case Right(parsed) =>
        // uncomment when AST format changes
        // Files.write(
        //   Path.of("examples/calendar_advanced.ast"),
        //   parsed.asJson.toString.getBytes(StandardCharsets.UTF_8)
        // )
        decode[NonEmptyList[Term]](astStr) match
          // check if AST matches expectation
          case Right(ast) =>
            assertEquals(parsed, ast);

          case Left(err) => fail(err.show)
  }
