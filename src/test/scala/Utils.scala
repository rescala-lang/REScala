package lore.test.util
import munit.FunSuite
import cats.parse.{Parser => P}
import cats.implicits._
import lore.AST.Term
import lore.backends.traverseFromNode
import lore.optics._

class ParserSuite extends FunSuite:
  def assertParses[A](p: P[A], expr: String): Unit =
    p.parseAll(expr) match {
      case Right(_) => ()
      case Left(x)  => fail(x.show)
    }

  def assertParsingResult(
      parser: P[Term],
      input: String,
      result: Term
  )(implicit
      loc: munit.Location
  ): Unit =
    parser.parseAll(input) match
      case Left(error) => fail(error.show)
      case Right(ast) =>
        assertEquals(
          traverseFromNode(ast, sourcePosLens.replace(None)),
          result
        )

  def assertParsingResult[A](
      parser: P[A],
      input: String,
      result: A
  )(implicit
      loc: munit.Location
  ): Unit =
    parser.parseAll(input) match
      case Left(error) => fail(error.show)
      case Right(ast) =>
        assertEquals(ast, result)
