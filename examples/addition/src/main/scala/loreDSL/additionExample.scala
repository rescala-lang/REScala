package loreDSL

import lore.DSL.*

object additionExample:
  @main def main: Unit =
    val a: Source[Int] = Source(0)
    val b: Derived[Int] = Derived { a() + a() }
    val add10 = Interaction[Int, Int]
      .requires((curr, _) => curr < 20)
      .modifies(a)
      .executes((curr, _) => curr + 10)
//      .requires((curr, _) => curr < 20)
//      .modifies(a)
//      .requires(() => false)
//      .executes((curr) => curr + 10)

    println(s"a: ${a.now}, b: ${b.now}")

    add10(0)
    println(s"a: ${a.now}, b: ${b.now}")

    add10(0)
    println(s"a: ${a.now}, b: ${b.now}")

    add10(0)
    println(s"a: ${a.now}, b: ${b.now}")
  end main
end additionExample