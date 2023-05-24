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

    Parser.disjunction.parseAll("x || true && false") match
      case Right(TDisj(TVar("x"), TConj(TTrue, TFalse))) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x           => fail(s"Failed to parse as conjunction: $x")

    Parser.disjunction.parseAll("false || true && foo") match
      case Right(TDisj(TFalse, TConj(TTrue, TVar("foo")))) => ()
      case Left(error) => fail(error.expected.head.toString)
      case x           => fail(s"Failed to parse as conjunction: $x")

    assertParsingResult(
      Parser.conjunction,
      "a1 in all_appointments && a2 in all_appointments && get_start(a2) < get_end(a1)",
      TConj(
        TInSet(TVar("a1"), TVar("all_appointments")),
        TConj(
          TInSet(TVar("a2"), TVar("all_appointments")),
          TLt(
            TFunC("get_start", List(TVar("a2"))),
            TFunC("get_end", List(TVar("a1")))
          )
        )
      )
    )

  }

  test("inequality") {
    val p = Parser.inequality

    p.parseAll("false != true") match
      case Right(TIneq(TFalse, TTrue)) => ()
      case Left(error)                 => fail(error.show)
      case x => fail(s"Failed to parse inequality: $x")

    Parser.conjunction.parseAll("true != false && true") match
      case Right(TConj(TIneq(TTrue, TFalse), TTrue)) => ()
      case Left(error) => fail(error.expected.toString)
      case x           => fail(s"Failed to parse inequality: $x")

    Parser.disjunction.parseAll("true != false && true || x && y") match
      case Right(
            TDisj(
              TConj(TIneq(TTrue, TFalse), TTrue),
              TConj(TVar("x"), TVar("y"))
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
      Parser.conjunction,
      "false == false && foo()",
      TConj(TEq(TFalse, TFalse), TFunC("foo", List()))
    )

    assertParsingResult(
      Parser.disjunction,
      "false || true && foo() == false",
      TDisj(
        TFalse,
        TConj(
          TTrue,
          TEq(TFunC("foo", List()), TFalse)
        )
      )
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

  test("bi-implication") {
    assertParsingResult(
      Parser.term,
      "a ==> c <==> b",
      TBImpl(TImpl(TVar("a"), TVar("c")), TVar("b"))
    )
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
        TDisj(
          TFalse,
          TConj(TTrue, TEq(TFunC("foo", List()), TFalse))
        )
      )
    )

    assertParsingResult(
      p,
      "false != true ==> false || true && foo() == false",
      TImpl(
        TIneq(TFalse, TTrue),
        TDisj(
          TFalse,
          TConj(
            TTrue,
            TEq(
              TFunC("foo", List()),
              TFalse
            )
          )
        )
      )
    )

    assertParsingResult(
      p,
      "(thisUser == user ==> is_correct(user,pass))",
      TParens(
        TImpl(
          TEq(TVar("thisUser"), TVar("user")),
          TFunC("is_correct", Seq(TVar("user"), TVar("pass")))
        )
      )
    )
  }

  test("parentheses") {
    val p = Parser.booleanExpr

    assertParsingResult(
      p,
      "(true || false) ==> false",
      TImpl(TParens(TDisj(TTrue, TFalse)), TFalse)
    )

    assertParsingResult(
      p,
      "false || true ==> (false)",
      TImpl(TDisj(TFalse, TTrue), TParens(TFalse))
    )

    assertParsingResult(
      p,
      "false || (true && foo()) == false",
      TDisj(
        TFalse,
        TEq(
          TParens(TConj(TTrue, TFunC("foo", List()))),
          TFalse
        )
      )
    )

    assertParsingResult(
      p,
      "false != (true ==> false)",
      TIneq(TFalse, TParens(TImpl(TTrue, TFalse)))
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

    assertParsingResult(
      Parser.booleanExpr,
      "(a, u) in invitations",
      TInSet(
        TTuple(NonEmptyList.fromListUnsafe(List(TVar("a"), TVar("u")))),
        TVar("invitations")
      )
    )

  }

  test("exists") {
    assertParsingResult(
      Parser.term,
      "exists h: History :: ph.toSet == old(ph.toSet.union(Set(h)))",
      TExists(
        NonEmptyList.one(TArgT("h", SimpleType("History", List()))),
        TEq(
          TFCall(TVar("ph"), "toSet", List()),
          TFunC(
            "old",
            List(
              TFCall(
                TFCall(TVar("ph"), "toSet", List()),
                "union",
                List(TFunC("Set", List(TVar("h"))))
              )
            )
          )
        )
      )
    )
  }

  test("forall") {
    assertParsingResult(
      Parser.boolParens,
      "(exists o: Order :: contains(orders, o) && get_o_num(o) == get_ol_o_num(ol))",
      TParens(
        TExists(
          NonEmptyList.one(TArgT("o", SimpleType("Order", List()))),
          TConj(
            TFunC("contains", List(TVar("orders"), TVar("o"))),
            TEq(
              TFunC("get_o_num", List(TVar("o"))),
              TFunC("get_ol_o_num", List(TVar("ol")))
            )
          )
        )
      )
    )

    assert(
      Parser.term
        .parseAll(
          """ol in (setminus(toSet(orderLines), seqToSet(newOrderLines)))"""
        )
        .isRight
    )
    assert(
      Parser.term
        .parseAll("""forall ol: OrderLine, o: Order :: ol in (setminus(toSet(orderLines), seqToSet(newOrderLines))) && contains(old(orders), o) && get_ol_o_num(ol) == get_o_num(o) && o != order_ ==>
         (get_ol_del_date(ol) == 0 <==> get_o_c_id(o) == 0)""")
        .isRight
    )

    assertParsingResult(
      Parser.neg,
      "!(exists o: Order :: contains(orders, o) && get_o_num(o) == get_ol_o_num(ol))",
      TNeg(
        TParens(
          TExists(
            NonEmptyList.one(TArgT("o", SimpleType("Order", List()))),
            TConj(
              TFunC("contains", List(TVar("orders"), TVar("o"))),
              TEq(
                TFunC("get_o_num", List(TVar("o"))),
                TFunC("get_ol_o_num", List(TVar("ol")))
              )
            )
          )
        )
      )
    )

    assertParsingResult(
      Parser.booleanExpr,
      "forall a: Appointment, u: User :: (a, u) in invitations ==> a in all_appointments",
      TForall(
        vars = NonEmptyList.fromListUnsafe(
          List(
            TArgT("a", SimpleType("Appointment", List.empty)),
            TArgT("u", SimpleType("User", List.empty))
          )
        ),
        triggers = List.empty,
        body = TImpl(
          TInSet(
            TTuple(NonEmptyList.fromListUnsafe(List(TVar("a"), TVar("u")))),
            TVar("invitations")
          ),
          TInSet(TVar("a"), TVar("all_appointments"))
        )
      )
    )

    val body = TForall(
      NonEmptyList.fromListUnsafe(
        List(
          TArgT("a1", SimpleType("Appointment", List.empty)),
          TArgT("a2", SimpleType("Appointment", List.empty))
        )
      ),
      triggers = List.empty,
      body = TImpl(
        TConj(
          TInSet(TVar("a1"), TVar("all_appointments")),
          TConj(
            TInSet(TVar("a2"), TVar("all_appointments")),
            TConj(
              TLt(
                TFunC("get_start", List(TVar("a2"))),
                TAdd(
                  TFunC("get_end", List(TVar("a1"))),
                  TNum(30)
                )
              ),
              TEq(
                TFunC("get_room", List(TVar("a1"))),
                TFunC("get_room", List(TVar("a2")))
              )
            )
          )
        ),
        TEq(TVar("a1"), TVar("a2"))
      )
    )

    assertParsingResult(
      Parser.forall,
      "forall a1: Appointment, a2: Appointment ::  a1 in all_appointments && a2 in all_appointments && get_start(a2) < get_end(a1) + 30 && get_room(a1) == get_room(a2) ==> a1 == a2",
      body
    )

    assertParsingResult(
      Parser.forall,
      """|forall a1: Appointment, a2: Appointment ::  a1 in all_appointments &&
         |a2 in all_appointments && get_start(a2) < get_end(a1) + 30 &&
         |   get_room(a1) == get_room(a2) ==> a1 == a2""".stripMargin,
      body
    )

    assertParsingResult(
      Parser.invariant,
      "invariant forall a1: Appointment, a2: Appointment ::  a1 in all_appointments && a2 in all_appointments && get_start(a2) < get_end(a1) + 30 && get_room(a1) == get_room(a2) ==> a1 == a2",
      TInvariant(body)
    )

  }
}
