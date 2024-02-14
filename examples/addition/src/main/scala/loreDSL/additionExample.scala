package loreDSL

import lore.DSL.*

object additionExample:
  @main def main(): Unit =
    val integerSource: Source[Int] = Source(0)
    val integerAdditionSource: Source[Int] = Source(4 + 2)
    val integerSubtractionSource: Source[Int] = Source(4 - 2)
    val integerMultiplicationSource: Source[Int] = Source(4 * 2)
    val integerDivisionSource: Source[Int] = Source(4 / 2)
    val stringSource: Source[String] = Source("abc")
    val boolTrueSource: Source[Boolean] = Source(true)
    val boolFalseSource: Source[Boolean] = Source(false)
    val boolNotSource: Source[Boolean] = Source(!false)
    val boolAndSource1: Source[Boolean] = Source(true && true)
    val boolAndSource2: Source[Boolean] = Source(true && false)
    val boolOrSource1: Source[Boolean] = Source(true || false)
    val boolOrSource2: Source[Boolean] = Source(false || false)
//    val integerDerived: Derived[Int] = Derived { integerSource() + integerSource() }
//    val integerInteraction = Interaction[Int, Int]
//      .requires((curr, _) => curr < 20)
//      .modifies(integerSource)
//      .executes((curr, _) => curr + 10)

//    println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")

//    integerInteraction(0)
//    println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")

//    integerInteraction(0)
//    println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")

//    integerInteraction(0)
//    println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")
  end main
end additionExample