package lore.test.util
import cats.implicits.*
import cats.parse.Parser as P
import lore.ast.Term
import lore.backends.traverseFromNode
import lore.optics.*
import munit.FunSuite

class ParserSuite extends FunSuite {
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
    parser.parseAll(input) match {
      case Left(error) => fail(error.show)
      case Right(ast) =>
        assertEquals(
          traverseFromNode(ast, sourcePosLens.replace(None)),
          result
        )
    }

  def assertParsingResult[A](
      parser: P[A],
      input: String,
      result: A
  )(implicit
      loc: munit.Location
  ): Unit =
    parser.parseAll(input) match {
      case Left(error) => fail(error.show)
      case Right(ast) =>
        assertEquals(ast, result)
    }
}
