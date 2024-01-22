package loreDSL

import lore.DSL.*

object additionExample:
  @main def main: Unit =
    val firstRealVariable: Source[Int] = Source(0)
    val secondRealVariable: Derived[Int] = Derived { firstRealVariable() + firstRealVariable() }
    val thirdRealVariable = Interaction[Int, Int]
//      .requires((curr, _) => curr < 20)
//      .modifies(firstRealVariable)
//      .executes((curr, _) => curr + 10)
//      .requires((curr, _) => curr < 20)
//      .modifies(firstRealVariable)
//      .requires(() => false)
//      .executes((curr) => curr + 10)

//    println(s"firstRealVariable: ${firstRealVariable.now}, secondRealVariable: ${secondRealVariable.now}")

//    add10(0)
//    println(s"firstRealVariable: ${firstRealVariable.now}, secondRealVariable: ${secondRealVariable.now}")

//    add10(0)
//    println(s"firstRealVariable: ${firstRealVariable.now}, secondRealVariable: ${secondRealVariable.now}")

//    add10(0)
//    println(s"firstRealVariable: ${firstRealVariable.now}, secondRealVariable: ${secondRealVariable.now}")
  end main
end additionExample