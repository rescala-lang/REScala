package fr
import minitest._
import AST._
import fr.Parser
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse._, ScalaWhitespace._

object BooleanExpressionParsing extends SimpleTestSuite {
  test("disjunction") {
    def p[_: P] = P(Parser.disjunction ~ End)

    val expr = "x || true"
    parse(expr, p(_)) match {
      case Success(Disjunction(_,ID(_,_), True(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse disjunction: $x")
    }

    val expr2 = "x || true || false"
    parse(expr2, p(_)) match {
      case Success(Disjunction(_, ID(_,_), Disjunction(_, True(_), False(_))), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse disjunction: $x")
    }
  }

  test("conjunction") {
    def p[_: P] = P(Parser.conjunction ~ End)

    val expr = "x && true"
    parse(expr, p(_)) match {
      case Success(AST.Conjunction(_, AST.ID(_,_), AST.True(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as conjunction: $x")
    }

    val expr2 = "x && true && false"
    parse(expr2, p(_)) match {
      case Success(Conjunction(_, ID(_,_), Conjunction(_, True(_), False(_))), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as conjunction: $x")
    }

    parse("x || true && false", p(_)) match {
      case Success(Conjunction(_, Disjunction(_, ID(_,_), True(_)), False(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as conjunction: $x")
    }

    parse("false || true && foo", p(_)) match {
      case Success(Conjunction(0,_,_), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as conjunction: $x")
    }
  }

  test("inequality") {
    def p[_: P] = P(Parser.inequality ~ End)

    parse("false != true", p(_)) match {
      case Success(Inequality(0, False(_), True(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse inequality: $x")
    }

    parse("true != false && true", p(_)) match {
      case Success(Inequality(_,
        True(_),
        Conjunction(_, False(_), True(_))),
        index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse inequality: $x")
    }

    parse("true != false && true || x && foo()", p(_)) match {
      case Success(Inequality(_, _,_), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse inequality: $x")
    }
  }

  test("equality") {
    def p[_: P] = P(Parser.equality ~ End)

    parse("true == false", p(_)) match {
      case Success(Equality(_, True(_), False(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as equality: $x")
    }

    parse("true == false && foo()", p(_)) match {
      case Success(Equality(0, True(_), Conjunction(8, False(_), Call(_,_,_))),
      index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as equality: $x")
    }

    parse("false || true && foo() == false", p(_)) match {
      case Success(Equality(0, _,_),
      index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as equality: $x")
    }

    parse("size(d) == size(d2).max", p(_)) match {
      case Success(Equality(0, _,_),
      index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as equality: $x")
    }
  }

  test("implication") {
    def p[_: P] = P(Parser.implication ~ End)

    parse("true ==> false", p(_)) match {
      case Success(Implication(0, True(_), False(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as implication: $x")
    }

    parse("false || true ==> false", p(_)) match {
      case Success(Implication(0, _, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as implication: $x")
    }

    parse("false ==> false || true && foo() == false", p(_)) match {
      case Success(Implication(0, _, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as implication: $x")
    }

    parse("false != true ==> false || true && foo() == false", p(_)) match {
      case Success(i @ Implication(0, _, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as implication: $x")
    }
  }

  test("parentheses") {
    def p[_: P] = P(Parser.booleanExpression ~ End)

    parse("(true || false) ==> false", p(_)) match {
      case Success(Implication(0, _,_), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as boolean expression: $x")
    }

    parse("false || true ==> (false)", p(_)) match {
      case Success(Implication(0, _, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as boolean expression: $x")
    }

    parse("false || (true && foo()) == false", p(_)) match {
      case Success(Equality(0, _, _), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as boolean expression: $x")
    }

    parse("false != (true ==> false)", p(_)) match {
      case Success(Inequality(0, _, Implication(10,_,_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as boolean expression: $x")
    }

    parse("false != (true ==> false", p(_)) match {
      case f: Failure => ()
      case x => fail("This should fail!")
    }
  }

  test("number comparison") {
    def p[_: P] = P(Parser.booleanExpression ~ End)

    parse("a >= 0", p(_)) match {
      case Success(Geq(0, _,_), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as number comparison: $x")
    }

    parse("foo(bar) < 0 == false", p(_)) match {
      case Success(Equality(0, Lt(_,_,_), False(_)), index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse as number comparison: $x")
    }
  }
}