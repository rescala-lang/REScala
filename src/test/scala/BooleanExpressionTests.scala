package lore
import minitest._
import lore.AST._
import cats.parse.{Parser => P}
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList

def printExp(e: NonEmptyList[Expectation]) =
  e.toString

object BooleanExpressionParsing extends SimpleTestSuite {
  test("disjunction") {
    val p = Parser.disjunction

    val expr = " x || true"
    p.parseAll(expr) match
      case Right(TDisj(TVar("x"), TTrue)) => ()
      case Left(error)                    => fail(error.expected.head.toString)
      case x => fail(s"Failed to parse as disjunction: $x")

    val expr2 = "x || true || false"
    p.parseAll(expr2) match
      case Right(TDisj(_, _)) => ()
      case Left(error)        => fail(error.expected.head.toString)
      case x                  => fail(s"Failed to parse as disjunction: $x")
  }

  test("conjunction") {
    val p = Parser.conjunction

    val expr = "x && true"
    p.parseAll(expr) match
      case Right(TConj(TVar("x"), TTrue)) => ()
      case Left(error)                    => fail(error.expected.toString)
      case x => fail(s"Failed to parse as conjunction: $x")

    val expr2 = "x && true && false"
    p.parseAll(expr2) match
      case Right(TConj(TVar(_), TConj(TTrue, TFalse))) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x           => fail(s"Failed to parse as conjunction: $x")

    p.parseAll("x || true && false") match
      case Right(TConj(TDisj(TVar("x"), TTrue), TFalse)) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x           => fail(s"Failed to parse as conjunction: $x")

    p.parseAll("false || true && foo") match
      case Right(TConj(TDisj(TFalse, TTrue), TVar("foo"))) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x           => fail(s"Failed to parse as conjunction: $x")
  }

  test("inequality") {
    val p = Parser.inequality

    p.parseAll("false != true") match
      case Right(TIneq(TFalse, TTrue)) => ()
      case Left(error)                 => fail(error.expected.toString)
      case x => fail(s"Failed to parse inequality: $x")

    p.parseAll("true != false && true") match
      case Right(TIneq(TTrue, TConj(TFalse, TTrue))) => ()
      case Left(error) => fail(error.expected.toString)
      case x           => fail(s"Failed to parse inequality: $x")

    p.parseAll("true != false && true || x && y") match
      case Right(
            TIneq(
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
            )
          ) =>
        ()
      case Left(error) => fail(error.expected.toString)
      case x           => fail(s"Failed to parse inequality: $x")

  }

  test("equality") {
    val p = Parser.equality

    p.parseAll("true == false") match
      case Right(TEq(TTrue, TFalse)) => ()
      case Left(error)               => fail(printExp(error.expected))
      case x                         => fail(s"Failed to parse as equality: $x")

    assertResult(Right(TEq(TTrue, TConj(TFalse, TFunC("foo", List()))))) {
      p.parseAll("true == false && foo()")
    }

    assertResult(
      Right(
        TEq(
          TConj(
            TDisj(TFalse, TTrue),
            TFunC("foo", List())
          ),
          TFalse
        )
      )
    ) {
      p.parseAll("false || true && foo() == false")
    }

    p.parseAll("size(d) == size(d2).max") match
      case Right(
            TEq(
              TFunC("size", _),
              TFAcc(TFunC("size", _), "max", List())
            )
          ) =>
        ()
      case Left(error) => fail(printExp(error.expected))
      case x           => fail(s"Failed to parse as equality: $x")

  }

//   test("implication") {
//     def p[_: P] = P(Parser.implication ~ End)

//     parse("true ==> false") match {
//       case Success(Implication(0, True(_), False(_)), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as implication: $x")
//     }

//     parse("false || true ==> false") match {
//       case Success(Implication(0, _, _), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as implication: $x")
//     }

//     parse("false ==> false || true && foo() == false") match {
//       case Success(Implication(0, _, _), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as implication: $x")
//     }

//     parse("false != true ==> false || true && foo() == false") match {
//       case Success(i @ Implication(0, _, _), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as implication: $x")
//     }
//   }

//   test("parentheses") {
//     def p[_: P] = P(Parser.booleanExpression ~ End)

//     parse("(true || false) ==> false") match {
//       case Success(Implication(0, _,_), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false || true ==> (false)") match {
//       case Success(Implication(0, _, _), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false || (true && foo()) == false") match {
//       case Success(Equality(0, _, _), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false != (true ==> false)") match {
//       case Success(Inequality(0, _, Implication(10,_,_)), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as boolean expression: $x")
//     }

//     parse("false != (true ==> false") match {
//       case f: Failure => ()
//       case x => fail("This should fail!")
//     }
//   }

//   test("number comparison") {
//     def p[_: P] = P(Parser.booleanExpression ~ End)

//     parse("a >= 0") match {
//       case Success(Geq(0, _,_), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as number comparison: $x")
//     }

//     parse("foo(bar) < 0 == false") match {
//       case Success(Equality(0, Lt(_,_,_), False(_)), index) => ()
//       case Left(error) => fail(printExp(error.expected))
//       case x => fail(s"Failed to parse as number comparison: $x")
//     }
//   }
}
