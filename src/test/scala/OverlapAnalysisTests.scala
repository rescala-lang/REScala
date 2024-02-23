package lore.backends
import lore.AST._
import munit.FunSuite
import lore.backends._
import cats.implicits._
import cats.data.NonEmptyList
import lore.Parser
import OverlapAnalysis._
import lore.backends.flattenInteractions

class OverlapAnalysisTests extends FunSuite {
  // simple reaches test
  test("reaches") {
    val prog = """|val a: Source[Int] = Source(0)
                  |val b: Source[Int] = Source(2)
                  |val c: Derived[Int] = Derived{a + b}
                  |val d: Derived[Int] = Derived{a}
                  |val e: Derived[Int] = Derived{d}
                  |
                  |val i: Unit = Interaction[Int][Int]
                  |  .executes{0}
                  |  .modifies(a)
                  |val j: Unit = Interaction[Int][Int]
                  |  .executes{0}
                  |  .modifies(b)""".stripMargin
    val ast: NonEmptyList[Term] = Parser.parse(prog) match {
      case Left(e)      => throw Exception(e.show)
      case Right(value) => value
    }
    val ctx: CompilationContext =
      flattenInteractions(CompilationContext(ast.toList))

    assertEquals(
      reaches(ctx.interactions("i"))(using ctx),
      Set("a", "c", "d", "e")
    )
    assertEquals(reaches(ctx.interactions("j"))(using ctx), Set("b", "c"))
  }
}
