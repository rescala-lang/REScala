package lore
import minitest._
import lore.AST._
import cats.parse

object SimpleParsing extends SimpleTestSuite:
  def assertParses[A](p: parse.Parser[A], expr: String): Unit =
    p.parseAll(expr) match {
      case Right(_) => ()
      case Left(x)  => fail(x.toString)
    }

  test("function call") {
    assertResult(Right(TFunC("foo", List(TNum(1), TNum(2))))) {
      Parser.functionCall.parseAll("foo(1,2)")
    }
  }

  test("id") {
    assertResult(Right("a")) {
      Parser.id.parseAll("a")
    }
  }

  test("argT") {
    assertResult(Right(TArgT("a", Type("Int", List())))) {
      Parser.argT.parseAll("a : Int")
    }
  }

  test("binding left") {
    assertResult(Right(TArgT("a", Type("Int", List())))) {
      Parser.bindingLeftSide.parseAll("val a : Int")
    }
  }

  test("binding") {
    assertResult(Right(TAbs("a", Type("Int", List()), TNum(12)))) {
      Parser.binding.parseAll("val a : Int = 12")
    }
  }

  test("field access") {
    assertResult(Right(TFAcc(TVar("foo"), "bar", List(TNum(1), TFalse)))) {
      Parser.fieldAcc.parseAll("foo.bar(1, false)")
    }

    assertResult(
      Right(
        TFAcc(
          TFAcc(
            TVar("foo"),
            "bar",
            List(TEq(TTrue, TFalse))
          ),
          "baz",
          List()
        )
      )
    ) {
      Parser.fieldAcc.parseAll("foo.bar(true == false).baz")
    }

    assertResult(Right(TFAcc(TFunC("size", List(TVar("d2"))), "max", List()))) {
      Parser.fieldAcc.parseAll("size(d2).max")
    }
  }

  test("arithmetic expression") {
    val p = Parser.arithmExpr
    val expr = "1 + 5 * 20 / (4 + 10)"
    assertResult(
      Right(
        TAdd(TNum(1), TMul(TNum(5), TDiv(TNum(20), TAdd(TNum(4), TNum(10)))))
      )
    ) {
      p.parseAll(expr)
    }
  }

  test("reactive") {
    val p = Parser.reactive
    assertResult(Right(TSource(TFunC("AWSet", List())))) {
      p.parseAll("Source(AWSet())")
    }

    assertResult(
      Right(
        TDerived(
          TFAcc(
            TFAcc(TVar("work"), "toSet", List()),
            "union",
            List(TFAcc(TVar("vacation"), "toSet", List()))
          )
        )
      )
    ) {
      p.parseAll("Derived{ work.toSet.union(vacation.toSet) }")
    }
  }

  test("named reactive") {
    val p = Parser.binding
    assertResult(
      Right(
        TAbs(
          "a",
          Type("Source", List(Type("Calendar", List()))),
          TSource(TFunC("AWSet", List()))
        )
      )
    ) {
      p.parseAll("val a: Source[Calendar] = Source(AWSet())")
    }

    assertResult(
      Right(
        TAbs(
          "a",
          Type("Derived", List(Type("Set", List(Type("Appointment", List()))))),
          TDerived(
            TFAcc(
              TFAcc(TVar("work"), "toSet", List()),
              "union",
              List(TFAcc(TVar("vacation"), "toSet", List()))
            )
          )
        )
      )
    ) {
      p.parseAll(
        "val a: Derived[Set[Appointment]]   = Derived{ work.toSet.union(vacation.toSet) }"
      )
    }
  }

  test("typename") {
    val expr = "List[Int]"
    val p = Parser.typeName
    assertParses(p, expr)

    val expr2 = "AWSet[Appointment]"
    assertParses(p, expr2)

    val expr3 = "Int"
    assertParses(p, expr3)
  }

  test("complex typename") {
    val expr = "Map[Int, String]"
    val p = Parser.typeName
    assertParses(p, expr)
  }

  test("type alias") {
    val expr = "type Calendar = AWSet[Appointment]"
    val p = Parser.typeAlias
    assertResult(
      Right(
        TTypeAl(
          "Calendar",
          Type("AWSet", List(Type("Appointment", List.empty)))
        )
      )
    ) {
      p.parseAll(expr)
    }
  }
