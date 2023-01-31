package lore
import cats.implicits._
import lore.AST._
import lore.test.util.ParserSuite

class SimpleParsing extends ParserSuite:
  test("function call") {
    assertParsingResult(
      Parser.functionCall,
      "foo(1,2)",
      TFunC("foo", List(TNum(1), TNum(2)))
    )
  }

  test("id") {
    assertParsingResult(Parser.id, "a", "a")
  }

  test("argT") {
    assertParsingResult(
      Parser.argT,
      "a : Int",
      TArgT("a", SimpleType("Int", List()))
    )
  }

  test("binding left") {
    assertParsingResult(
      Parser.bindingLeftSide,
      "val a : Int",
      TArgT("a", SimpleType("Int", List()))
    )
  }

  test("binding") {
    assertParsingResult(
      Parser.binding,
      "val a : Int = 12",
      TAbs("a", SimpleType("Int", List()), TNum(12))
    )
  }

  test("field access") {
    assertParsingResult(
      Parser.fieldAcc,
      "foo.bar(1, false)",
      TFCall(TVar("foo"), "bar", List(TNum(1), TFalse))
    )

    assertParsingResult(
      Parser.fieldAcc,
      "foo.bar(true == false).baz",
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

    assertParsingResult(
      Parser.fieldAcc,
      "size(d2).max",
      TFCall(TFunC("size", List(TVar("d2"))), "max", List())
    )

    assert(
      Parser.fieldAcc
        .parse("UI.vacationDialog.onConfirm{a => add_vacation.apply(a)}")
        .isRight
    )
  }

  test("arithmetic expression") {
    val p = Parser.arithmExpr
    val expr = "1 + 5 * 20 / (4 + 10)"

    assertParsingResult(
      p,
      expr,
      TAdd(TNum(1), TMul(TNum(5), TDiv(TNum(20), TAdd(TNum(4), TNum(10)))))
    )
  }

  test("reactive") {
    val p = Parser.reactive
    assertParsingResult(p, "Source(AWSet())", TSource(TFunC("AWSet", List())))

    assertParsingResult(
      p,
      "Derived{ work.toSet.union(vacation.toSet) }",
      TDerived(
        TFCall(
          TFCall(TVar("work"), "toSet", List()),
          "union",
          List(TFCall(TVar("vacation"), "toSet", List()))
        )
      )
    )
  }

  test("named reactive") {
    val p = Parser.binding

    assertParsingResult(
      p,
      "val a: Source[Calendar] = Source(AWSet())",
      TAbs(
        "a",
        SimpleType("Source", List(SimpleType("Calendar", List()))),
        TSource(TFunC("AWSet", List()))
      )
    )

    assertParsingResult(
      p,
      "val a: Derived[Set[Appointment]]   = Derived{ work.toSet.union(vacation.toSet) }",
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
    assertParsingResult(
      p,
      expr,
      TTypeAl(
        "Calendar",
        SimpleType("AWSet", List(SimpleType("Appointment", List.empty)))
      )
    )
  }

  test("lambda fun") {

    assertParsingResult(
      Parser.lambdaFun,
      "x => x",
      TArrow(TVar("x"), TVar("x"))
    )

    assertParsingResult(
      Parser.lambdaFun,
      "x => x => x",
      TArrow(TVar("x"), TArrow(TVar("x"), TVar("x")))
    )

    assertParsingResult(
      Parser.lambdaFun,
      "x => y => x + y",
      TArrow(TVar("x"), TArrow(TVar("y"), TAdd(TVar("x"), TVar("y"))))
    )
  }

  test("comment") {
    assertParsingResult(Parser.comment, "// this is a comment", ())

    val multilineString =
      """|// this is a comment
         |lalala""".stripMargin
    Parser.comment.parse(multilineString) match
      case Left(error) => fail(error.show)
      case Right(ast) =>
        assertEquals(
          ast,
          ("\nlalala", ())
        )
  }
