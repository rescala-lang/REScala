package lore
import minitest._
import lore.AST._
import cats.parse.{Parser => P}

object BooleanExpressionParsing extends SimpleTestSuite {
  test("disjunction") {
    val p = Parser.disjunction

    val expr = "x || true"
    p.parseAll(expr) match
      case Right(TDisj(TVar("x"), TTrue)) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x => fail(s"Failed to parse as disjunction: $x")
    

    val expr2 = "x || true || false"
    p.parseAll(expr2) match
      case Right(TDisj(_,_)) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x => fail(s"Failed to parse as disjunction: $x")
  }

  test("conjunction") {
    val p = Parser.conjunction

    val expr = "x && true"
    p.parse(expr) match
      case Right((parsed, TConj(TVar(_),TTrue))) => (println(parsed))
      case Left(error) => fail(error.expected.toString)
      case x => fail(s"Failed to parse as conjunction: $x")

    val expr2 = "x && true && false"
    p.parseAll(expr2) match
      case Right(TConj(TVar(_),TConj(TTrue, TFalse))) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x => fail(s"Failed to parse as conjunction: $x")

    p.parseAll("x || true && false") match
      case Right(TConj(TDisj(TVar("x"),TTrue), TFalse)) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x => fail(s"Failed to parse as conjunction: $x")

    p.parseAll("false || true && foo") match
      case Right(TConj(TDisj(TFalse, TTrue), TVar("foo"))) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x => fail(s"Failed to parse as conjunction: $x")
  }

  test("inequality") {
    val p = Parser.inequality

    p.parseAll("false != true") match
      case Right(TIneq(TFalse, TTrue)) => ()
      case Left(error) => fail(error.expected.toString)
      case x => fail(s"Failed to parse inequality: $x")
    

    p.parseAll("true != false && true") match
      case Right(TIneq(TTrue, TConj(TFalse, TTrue))) => ()
      case Left(error) => fail(error.expected.toString)
      case x => fail(s"Failed to parse inequality: $x")

    p.parseAll("true != false && true || x && y") match
      case Right(TIneq(
        TTrue,
        TConj(
            TFalse,
            TConj(
                TDisj(
                    TTrue,
                    TVar("x")
                ),
                TVar("y")
            )
        )
      )) => ()
      case Left(error) => fail(error.expected.toString)
      case x => fail(s"Failed to parse inequality: $x")

  }

//   test("equality") {
//     def p[_: P] = P(Parser.equality ~ End)

//     parse("true == false", p(_)) match {
//       case Success(Equality(_, True(_), False(_)), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as equality: $x")
//     }

//     parse("true == false && foo()", p(_)) match {
//       case Success(Equality(0, True(_), Conjunction(8, False(_), Call(_,_,_))),
//       index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as equality: $x")
//     }

//     parse("false || true && foo() == false", p(_)) match {
//       case Success(Equality(0, _,_),
//       index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as equality: $x")
//     }

//     parse("size(d) == size(d2).max", p(_)) match {
//       case Success(Equality(0, _,_),
//       index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as equality: $x")
//     }
//   }

//   test("implication") {
//     def p[_: P] = P(Parser.implication ~ End)

//     parse("true ==> false", p(_)) match {
//       case Success(Implication(0, True(_), False(_)), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as implication: $x")
//     }

//     parse("false || true ==> false", p(_)) match {
//       case Success(Implication(0, _, _), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as implication: $x")
//     }

//     parse("false ==> false || true && foo() == false", p(_)) match {
//       case Success(Implication(0, _, _), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as implication: $x")
//     }

//     parse("false != true ==> false || true && foo() == false", p(_)) match {
//       case Success(i @ Implication(0, _, _), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as implication: $x")
//     }
//   }

//   test("parentheses") {
//     def p[_: P] = P(Parser.booleanExpression ~ End)

//     parse("(true || false) ==> false", p(_)) match {
//       case Success(Implication(0, _,_), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false || true ==> (false)", p(_)) match {
//       case Success(Implication(0, _, _), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false || (true && foo()) == false", p(_)) match {
//       case Success(Equality(0, _, _), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false != (true ==> false)", p(_)) match {
//       case Success(Inequality(0, _, Implication(10,_,_)), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false != (true ==> false", p(_)) match {
//       case f: Failure => ()
//       case x => fail("This should fail!")
//     }
//   }

//   test("number comparison") {
//     def p[_: P] = P(Parser.booleanExpression ~ End)

//     parse("a >= 0", p(_)) match {
//       case Success(Geq(0, _,_), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as number comparison: $x")
//     }

//     parse("foo(bar) < 0 == false", p(_)) match {
//       case Success(Equality(0, Lt(_,_,_), False(_)), index) => ()
//       case f: Failure => fail(f.trace().longAggregateMsg)
//       case x => fail(s"Failed to parse as number comparison: $x")
//     }
//   }
}