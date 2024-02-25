package lore.dsl

import lore.dsl._

import rescala.default

object DSLTest {
  @main
  def main(): Unit = {
    val a: Source[Int] = Source(0)
    val b: Derived[Int] = Derived { a() + a() }
    
    Invariant("one")
    
    val add10 =
      Interaction[Int, Int]
        .requires((curr, _) => curr < 20)
        .modifies(a)
        .executes((curr, _) => curr + 10)
    
    Invariant("two")

    a observe(v => println(s"a: $v"))
    b observe(v => println(s"b: $v"))

    add10(0)
    add10(0)
    add10(0)
  }
}
