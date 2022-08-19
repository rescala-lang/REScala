package lore
import minitest._
import lore.AST._
import cats.parse.{Parser => P}
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import SimpleParsing.assertParses

def printExp(e: NonEmptyList[Expectation]) =
  e.toString

object BooleanExpressionParsing extends SimpleTestSuite {
  test("disjunction") {
    val p = Parser.disjunction

    val expr = "x || true"
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
              TFCall(TFunC("size", _), "max", List())
            )
          ) =>
        ()
      case Left(error) => fail(printExp(error.expected))
      case x           => fail(s"Failed to parse as equality: $x")

  }

  test("implication") {
    val p = Parser.implication

    assertResult(Right(TImpl(TTrue, TFalse))) {
      p.parseAll("true==>false")
    }

    assertResult(Right(TImpl(TDisj(TFalse, TTrue), TFalse))) {
      p.parseAll("false || true ==> false")
    }

    assertResult(
      Right(
        TImpl(
          TFalse,
          TEq(TConj(TDisj(TFalse, TTrue), TFunC("foo", List())), TFalse)
        )
      )
    ) {
      p.parseAll("false ==> false || true && foo() == false")
    }

    assertResult(
      Right(
        TImpl(
          TIneq(TFalse, TTrue),
          TEq(
            TConj(
              TDisj(TFalse, TTrue),
              TFunC("foo", List())
            ),
            TFalse
          )
        )
      )
    ) {
      p.parseAll("false != true ==> false || true && foo() == false")
    }

    assertResult(
      Right(
        TImpl(
          TEq(TVar("thisUser"), TVar("user")),
          TFunC("is_correct", Seq(TVar("user"), TVar("pass")))
        )
      )
    ) {
      p.parseAll("(thisUser == user ==> is_correct(user,pass))")
    }

  }

  test("parentheses") {
    val p = Parser.booleanExpr

    assertResult(Right(TImpl(TDisj(TTrue, TFalse), TFalse))) {
      p.parseAll("(true || false) ==> false")
    }

    assertResult(Right(TImpl(TDisj(TFalse, TTrue), TFalse))) {
      p.parseAll("false || true ==> (false)")
    }

    assertResult(
      Right(TEq(TDisj(TFalse, TConj(TTrue, TFunC("foo", List()))), TFalse))
    ) {
      p.parseAll("false || (true && foo()) == false")
    }

    assertResult(Right(TIneq(TFalse, TImpl(TTrue, TFalse)))) {
      p.parseAll("false != (true ==> false)")
    }

    // this should fail
    p.parseAll("false != (true ==> false") match
      case Left(e: cats.parse.Parser.Error) => ()
      case _                                => fail("This should fail!")
  }

  test("number comparison") {
    val p = Parser.booleanExpr

    assertResult(Right(TGeq(TVar("a"), TNum(0)))) {
      p.parseAll("a >= 0")
    }

    assertResult(
      Right(TEq(TLt(TFunC("foo", List(TVar("bar"))), TNum(0)), TFalse))
    ) {
      p.parseAll("foo(bar) < 0 == false")
    }
  }

  test("in set") {
    val p = Parser.inSet

    assertResult(Right(TInSet(TNum(12), TVar("a")))) {
      p.parseAll("12 in a")
    }

    assertParses(p, "foo(true) in X.mySet")
  }
}
