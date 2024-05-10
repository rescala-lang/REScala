package lore.dsl

import lore.dsl.Ex
import reactives.default.Var as Source
import reactives.default.Signal as Derived

object DSLTest {
  @main
  def main(): Unit = {
    val a1: Source[Int] = Source(0)
    val a2: Source[Int] = Source(0)
    val a3: Source[Int] = Source(0)

    val b: Derived[Int] = Derived {
      a1.value + a2.value + a3.value
    }

    Invariant {
      b.value < 25
    }

    val add10: BoundInteraction[Tuple1[Int], Tuple1[Source[Int]], Int] =
      Interaction[Int, Int]
        .requires[Int]((t, _) => t < 20)
        .modifies(a1)
        .executes((t: Int, _) => t + 10)
        .ensures[Int]((t, _) => t < 5)

    val x: BoundInteraction[(Int, Int), (Source[Int], Source[Int]), Int] = Interaction[Int, Int, Int]
      .modifies(a1, a2)
      .executes((t1: Int, t2: Int, _) => (t1 + 10, t2 + 10))

    val x2: BoundInteraction[(Int, Int), (Source[Int], Source[Int]), Int] = Interaction[Int, Int, Int]
      .executes((t1: Int, t2: Int, _) => (t1 + 10, t2 + 10))
      .modifies(a2, a3)

    val y = Interaction[Int, Int, Int, Int]
      .modifies(a1, a2, a3)
      .executes((t, _) => (t._1 + 10, t._2 + 10, t._3 + 10))

    a1 observe (v => println(s"a1: $v"))
    a2 observe (v => println(s"a2: $v"))
    a3 observe (v => println(s"a3: $v"))
    b observe (v => println(s"b: $v"))

    add10(0)
    add10(0)
    add10(0)

    x(0)
    x(0)
    x(0)

    x2(0)
    x2(0)
    x2(0)

    y(0)
    y(0)
    y(0)
  }
}

/*
case class Box[T](t: T)

class Test[S <: NonEmptyTuple, T <: NonEmptyTuple](val s: S, val t: T) {

  def append[A](a: A): Test[Append[S, A], Append[T, Box[A]]] = Test(s :* a, t :* Box(a))

}

val x: Test[Int *: Int *: (Int, Int), Box[Int] *: Box[Int] *: (Box[Int], Box[Int])] = Test((1, 2), (Box(1), Box(2)))
  .append(3)
  .append(4)
*/
