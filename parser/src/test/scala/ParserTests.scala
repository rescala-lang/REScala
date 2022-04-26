package fr
import minitest._
import AST._
import fr.Parser
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse._, ScalaWhitespace._

object ParserTestSuite extends SimpleTestSuite {
  test("arithmetic expression") {
    def p[_: P] = P(Parser.arithmeticExpression ~ End)
    val expr = "1 + 5 * 20 / (4 + 10)"
    parse(expr, p(_)) match {
      case Success(value: AST.ArithmeticExpression, index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse arithmetic expression: $x")
    }
  }
  test("typename") {
    val expr = "List[Int]"
    def p[_: P] = P(Parser.typeName ~ End)
    parse(expr, p(_)) match {
      case Success(value: AST.TypeName, index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse: $x")
    }
  }
  test("complex typename") {
    val expr = "Map[Int, String]"
    def p[_: P] = P(Parser.typeName ~ End)
    parse(expr, p(_)) match {
      case Success(value: AST.TypeName, index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse: $x")
    }
  }
}