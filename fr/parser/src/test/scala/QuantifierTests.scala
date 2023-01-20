package fr
import minitest._
import AST._
import fr.Parser
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse._, ScalaWhitespace._

object QuantifierParsing extends SimpleTestSuite {
  test("forall") {
    def p[_: P] = P(Parser.forall ~ End)

    parse("forall a: Int :: a >= 0", p(_)) match {
      case Success(Forall(0, _, None, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse forall: $x")
    }
  }

  test("with trigger") {
    def p[_: P] = P(Parser.forall ~ End)

    parse("forall a: Int :: {p(a)} a >= 0", p(_)) match {
      case Success(Forall(0, _, _, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse forall: $x")
    }
  }

  test("exists") {
    def p[_: P] = P(Parser.exists ~ End)

    parse("exists a: Bool :: b || (b ==> false)", p(_)) match {
      case Success(Exists(0, _, Disjunction(_, _, Implication(_,_,_))), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse forall: $x")
    }
  }

}