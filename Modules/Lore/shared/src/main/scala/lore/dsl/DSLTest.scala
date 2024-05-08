package lore.dsl

import lore.dsl.Ex
import reactives.default.*

object DSLTest {
  @main
  def main(): Unit = {
    val a1: Var[Int] = Var(0)
    val a2: Var[Int] = Var(0)
    val a3: Var[Int] = Var(0)

    val b: Signal[Int] = Signal {
      a1.value + a2.value + a3.value
    }

    Invariant {
      b.value < 25
    }


    val x = Interaction[Int, Int, Int]
      .modifies(a1, a2)
      .executes((t1: Int, t2: Int, _) => (t1 + 10, t2 + 10))

    val x2 = Interaction[Int, Int, Int]
      .executes((t1: Int, t2: Int, _) => (t1 + 10, t2 + 10))
      .modifies(a2, a3)

    val y = Interaction[Int, Int, Int, Int]
      .modifies(a1, a2, a3)
      .executes((t, _) => (t._1 + 10, t._2 + 10, t._3 + 10))

    val z = Interaction[Int, Int]
      .actsOn(Evt[Int]())
      .executes((t: Int, _) => t + 10)

    // a1 observe (v => println(s"a1: $v"))
    // a2 observe (v => println(s"a2: $v"))
    // a3 observe (v => println(s"a3: $v"))
    // b observe (v => println(s"b: $v"))

//    x(0)
//    x(0)
//    x(0)
//
//    x2(0)
//    x2(0)
//    x2(0)
//
//    y(0)
//    y(0)
//    y(0)
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
