package lore
import lore.AST._
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import lore.test.util.ParserSuite

def printExp(e: NonEmptyList[Expectation]) =
  e.toString

class BooleanExpressionParsing extends ParserSuite {
  test("disjunction") {
    assertParsingResult(
      Parser.disjunction,
      "x || true",
      TDisj(TVar("x"), TTrue())
    )

    assertParsingResult(
      Parser.term,
      "x || true || false",
      TDisj(TVar("x"), TDisj(TTrue(), TFalse()))
    )
  }

  test("conjunction") {
    val p = Parser.conjunction

    assertParsingResult(
      p,
      "x && true",
      TConj(TVar("x"), TTrue())
    )

    assertParsingResult(
      p,
      "x && true && false",
      TConj(TVar("x"), TConj(TTrue(), TFalse()))
    )

    assertParsingResult(
      Parser.term,
      "x || true && false",
      TDisj(TVar("x"), TConj(TTrue(), TFalse()))
    )

    assertParsingResult(
      Parser.term,
      "false || true && foo",
      TDisj(TFalse(), TConj(TTrue(), TVar("foo")))
    )

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
    assertParsingResult(
      Parser.term,
      "false != true",
      TIneq(TFalse(), TTrue())
    )

    assertParsingResult(
      Parser.term,
      "true != false && true",
      TConj(TIneq(TTrue(), TFalse()), TTrue())
    )

    assertParsingResult(
      Parser.term,
      "true != false && true || x && y",
      TDisj(
        TConj(TIneq(TTrue(), TFalse()), TTrue()),
        TConj(TVar("x"), TVar("y"))
      )
    )
  }

  test("equality") {
    assertParsingResult(
      Parser.term,
      "true == false",
      TEq(TTrue(), TFalse())
    )

    assertParsingResult(
      Parser.conjunction,
      "false == false && foo()",
      TConj(TEq(TFalse(), TFalse()), TFunC("foo", List()))
    )

    assertParsingResult(
      Parser.disjunction,
      "false || true && foo() == false",
      TDisj(
        TFalse(),
        TConj(
          TTrue(),
          TEq(TFunC("foo", List()), TFalse())
        )
      )
    )

    assertParsingResult(
      Parser.disjunction,
      "size(d) == size(d2).max",
      TEq(
        TFunC("size", List(TVar("d"))),
        TFCall(TFunC("size", List(TVar("d2"))), "max", List())
      )
    )

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

    assertParsingResult(p, "true==>false", TImpl(TTrue(), TFalse()))

    assertParsingResult(
      p,
      "false || true ==> false",
      TImpl(TDisj(TFalse(), TTrue()), TFalse())
    )

    assertParsingResult(
      p,
      "false ==> false || true && foo() == false",
      TImpl(
        TFalse(),
        TDisj(
          TFalse(),
          TConj(TTrue(), TEq(TFunC("foo", List()), TFalse()))
        )
      )
    )

    assertParsingResult(
      p,
      "false != true ==> false || true && foo() == false",
      TImpl(
        TIneq(TFalse(), TTrue()),
        TDisj(
          TFalse(),
          TConj(
            TTrue(),
            TEq(
              TFunC("foo", List()),
              TFalse()
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
      TImpl(TParens(TDisj(TTrue(), TFalse())), TFalse())
    )

    assertParsingResult(
      p,
      "false || true ==> (false)",
      TImpl(TDisj(TFalse(), TTrue()), TParens(TFalse()))
    )

    assertParsingResult(
      p,
      "false || (true && foo()) == false",
      TDisj(
        TFalse(),
        TEq(
          TParens(TConj(TTrue(), TFunC("foo", List()))),
          TFalse()
        )
      )
    )

    assertParsingResult(
      p,
      "false != (true ==> false)",
      TIneq(TFalse(), TParens(TImpl(TTrue(), TFalse())))
    )

    // this should fail
    p.parseAll("false != (true ==> false") match {
      case Left(e: cats.parse.Parser.Error) => ()
      case _                                => fail("This should fail!")
    }
  }

  test("number comparison") {
    val p = Parser.booleanExpr

    assertParsingResult(p, "a >= 0", TGeq(TVar("a"), TNum(0)))

    assertParsingResult(
      p,
      "foo(bar) < 0 == false",
      TEq(TLt(TFunC("foo", List(TVar("bar"))), TNum(0)), TFalse())
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

  test("forall with trigger") {
    assertParses(
      Parser.trigger,
      "{get_o_num(a)}"
    )
    assertParses(
      Parser.booleanExpr,
      "contains(orders, o) && get_o_num(o) == get_ol_o_num(ol)"
    )
    assertParses(
      Parser.booleanExpr,
      "forall a: Order :: {get_o_num(a), foo(a)} {contains(a)} contains(orders, o) && get_o_num(o) == get_ol_o_num(ol)"
    )
  }
}
