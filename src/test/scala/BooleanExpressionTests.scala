package lore
import lore.AST._
import cats.implicits._
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import lore.test.util.ParserSuite

def printExp(e: NonEmptyList[Expectation]) =
  e.toString

class BooleanExpressionParsing extends ParserSuite {
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
      case Left(error)                 => fail(error.show)
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

    assertParsingResult(
      p,
      "false == false && foo()",
      TEq(TFalse, TConj(TFalse, TFunC("foo", List())))
    )

    assertParsingResult(
      p,
      "false || true && foo() == false",
      TEq(TConj(TDisj(TFalse, TTrue), TFunC("foo", List())), TFalse)
    )

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

    assertParsingResult(p, "true==>false", TImpl(TTrue, TFalse))

    assertParsingResult(
      p,
      "false || true ==> false",
      TImpl(TDisj(TFalse, TTrue), TFalse)
    )

    assertParsingResult(
      p,
      "false ==> false || true && foo() == false",
      TImpl(
        TFalse,
        TEq(TConj(TDisj(TFalse, TTrue), TFunC("foo", List())), TFalse)
      )
    )

    assertParsingResult(
      p,
      "false != true ==> false || true && foo() == false",
      TImpl(
        TIneq(TFalse, TTrue),
        TEq(TConj(TDisj(TFalse, TTrue), TFunC("foo", List())), TFalse)
      )
    )

    assertParsingResult(
      p,
      "(thisUser == user ==> is_correct(user,pass))",
      TImpl(
        TEq(TVar("thisUser"), TVar("user")),
        TFunC("is_correct", Seq(TVar("user"), TVar("pass")))
      )
    )
  }

  test("parentheses") {
    val p = Parser.booleanExpr

    assertParsingResult(
      p,
      "(true || false) ==> false",
      TImpl(TDisj(TTrue, TFalse), TFalse)
    )

    assertParsingResult(
      p,
      "false || true ==> (false)",
      TImpl(TDisj(TFalse, TTrue), TFalse)
    )

    assertParsingResult(
      p,
      "false || (true && foo()) == false",
      TEq(TDisj(TFalse, TConj(TTrue, TFunC("foo", List()))), TFalse)
    )

    assertParsingResult(
      p,
      "false != (true ==> false)",
      TIneq(TFalse, TImpl(TTrue, TFalse))
    )

    // this should fail
    p.parseAll("false != (true ==> false") match
      case Left(e: cats.parse.Parser.Error) => ()
      case _                                => fail("This should fail!")
  }

  test("number comparison") {
    val p = Parser.booleanExpr

    assertParsingResult(p, "a >= 0", TGeq(TVar("a"), TNum(0)))

    assertParsingResult(
      p,
      "foo(bar) < 0 == false",
      TEq(TLt(TFunC("foo", List(TVar("bar"))), TNum(0)), TFalse)
    )
  }

  test("in set") {
    val p = Parser.inSet

    assertParsingResult(p, "12 in a", TInSet(TNum(12), TVar("a")))

    assertParses(p, "foo(true) in X.mySet")
  }
}
