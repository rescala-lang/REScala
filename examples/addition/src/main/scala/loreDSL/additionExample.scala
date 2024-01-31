package loreDSL

import lore.DSL.*

object additionExample:
  @main def main(): Unit =
    val integerSource: Source[Int] = Source(0)
    val stringSource: Source[String] = Source("abc")
    val boolSource: Source[Boolean] = Source(true)
//    val secondRealVariable: Derived[Int] = Derived { integerSource() + integerSource() }
//    val thirdRealVariable = Interaction[Int, Int]
//      .requires((curr, _) => curr < 20)
//      .modifies(integerSource)
//      .executes((curr, _) => curr + 10)
//      .requires((curr, _) => curr < 20)
//      .modifies(integerSource)
//      .requires(() => false)
//      .executes((curr) => curr + 10)

//    println(s"integerSource: ${integerSource.now}, secondRealVariable: ${secondRealVariable.now}")

//    add10(0)
//    println(s"integerSource: ${integerSource.now}, secondRealVariable: ${secondRealVariable.now}")

//    add10(0)
//    println(s"integerSource: ${integerSource.now}, secondRealVariable: ${secondRealVariable.now}")

//    add10(0)
//    println(s"integerSource: ${integerSource.now}, secondRealVariable: ${secondRealVariable.now}")
  end main
end additionExample