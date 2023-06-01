package lore
import lore.AST._
import cats.implicits._
import monocle.syntax.all._
import monocle.Lens
import munit.FunSuite
import monocle.macros.GenLens
import cats.parse.Caret
import cats.data.NonEmptyList
import lore.backends.traverseFromNode
import lore.optics._

class OpticsSuite extends FunSuite:
  test("Playground") {
    val a = Parser.prog.parseAll("((12))")
    val replaceSourcePos = Subtree.modify(sourcePosLens.replace(None))
    // println(Children.getAll(a.getOrElse(NonEmptyList.one(TVar("a"))).head))
    for
      parsed <- a
      replaced = parsed.map(replaceSourcePos)
      _ = assertEquals(
        replaced,
        NonEmptyList.one(TParens(TParens(TNum(12))))
      )
    yield ()
  }

  test("Playground2") {
    val a = Parser.prog.parseAll("((12))")
    val replaceSourcePos = Subtree.modify(sourcePosLens.replace(None))
    // println(Children.getAll(a.getOrElse(NonEmptyList.one(TVar("a"))).head))
    for
      parsed <- a
      replaced = parsed.map(n =>
        traverseFromNode(n, sourcePosLens.replace(None))
      )
      _ = assertEquals(
        replaced,
        NonEmptyList.one(TParens(TParens(TNum(12))))
      )
    yield ()
  }
