package lore
import lore.ast._
import munit.FunSuite
import cats.data.NonEmptyList
import lore.backends.traverseFromNode
import lore.optics.{_, given}

class OpticsSuite extends FunSuite {
  test("Playground") {
    val a                = Parser.prog.parseAll("((12))")
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
    // println(Children.getAll(a.getOrElse(NonEmptyList.one(TVar("a"))).head))
    for
      parsed <- a
      replaced = parsed.map(n =>
        traverseFromNode(n, sourcePosLens.replace(None))
      )
      _ = assertEquals(replaced, NonEmptyList.one(TParens(TParens(TNum(12)))))
      _ = assertEquals(children.getAll(replaced.head), List(TParens(TNum(12))))
    yield ()
  }
  test("Children lens") {
    val a = Parser.prog.parseAll("((12))")
    // println(Children.getAll(a.getOrElse(NonEmptyList.one(TVar("a"))).head))
    for
      parsed <- a
      replaced = parsed.map(n =>
        traverseFromNode(n, sourcePosLens.replace(None))
      )
      _ = assertEquals(replaced, NonEmptyList.one(TParens(TParens(TNum(12)))))
      _ = assertEquals(children.getAll(replaced.head), List(TParens(TNum(12))))
    yield ()
  }
  test("Children typeclass") {
    val a = Parser.prog.parseAll("((12))")
    // val replaceSourcePos = Subtree.modify(sourcePosLens.replace(None))
    // println(Children.getAll(a.getOrElse(NonEmptyList.one(TVar("a"))).head))
    for
      parsed <- a
      replaced = parsed.map(n =>
        traverseFromNode(n, sourcePosLens.replace(None))
      )
      _ = assertEquals(replaced, NonEmptyList.one(TParens(TParens(TNum(12)))))
      _ = assertEquals(replaced.head.children, List(TParens(TNum(12))))
    yield ()
  }
}
