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
    assertResult(Right(TArgT("a", SimpleType("Int", List())))) {
      Parser.argT.parseAll("a : Int")
    }
  }

  test("binding left") {
    assertResult(Right(TArgT("a", SimpleType("Int", List())))) {
      Parser.bindingLeftSide.parseAll("val a : Int")
    }
  }

  test("binding") {
    assertResult(Right(TAbs("a", SimpleType("Int", List()), TNum(12)))) {
      Parser.binding.parseAll("val a : Int = 12")
    }
  }

  test("field access") {
    assertResult(Right(TFCall(TVar("foo"), "bar", List(TNum(1), TFalse)))) {
      Parser.fieldAcc.parseAll("foo.bar(1, false)")
    }

    assertResult(
      Right(
        TFCall(
          TFCall(
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

    assertResult(
      Right(TFCall(TFunC("size", List(TVar("d2"))), "max", List()))
    ) {
      Parser.fieldAcc.parseAll("size(d2).max")
    }

    assert(
      Parser.fieldAcc
        .parse("UI.vacationDialog.onConfirm{a => add_vacation.apply(a)}")
        .isRight
    )
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
          TFCall(
            TFCall(TVar("work"), "toSet", List()),
            "union",
            List(TFCall(TVar("vacation"), "toSet", List()))
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
          SimpleType("Source", List(SimpleType("Calendar", List()))),
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
          SimpleType(
            "Derived",
            List(SimpleType("Set", List(SimpleType("Appointment", List()))))
          ),
          TDerived(
            TFCall(
              TFCall(TVar("work"), "toSet", List()),
              "union",
              List(TFCall(TVar("vacation"), "toSet", List()))
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
          SimpleType("AWSet", List(SimpleType("Appointment", List.empty)))
        )
      )
    ) {
      p.parseAll(expr)
    }
  }

  test("lambda fun") {

    assertResult(
      Right(TArrow(TVar("x"), TVar("x")))
    ) {
      Parser.lambdaFun.parseAll("x => x")
    }

    assertResult(
      Right(TArrow(TVar("x"), TArrow(TVar("x"), TVar("x"))))
    ) {
      Parser.lambdaFun.parseAll("x => x => x")
    }

    assertResult(
      Right(TArrow(TVar("x"), TArrow(TVar("y"), TAdd(TVar("x"), TVar("y")))))
    ) {
      Parser.lambdaFun.parseAll("x => y => x + y")
    }
  }

  test("comment") {
    assertResult(
      Right(())
    ) {
      Parser.comment.parseAll("// this is a comment")
    }

    assertResult(
      Right(("\nlalala", ()))
    ) {
      Parser.comment.parse("""|// this is a comment
                              |lalala""".stripMargin)
    }
  }
