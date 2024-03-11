package loreDSL

import lore.DSL.*

object additionExample:
  @main def main(): Unit =
    val testInt: Int = 0
    val integerReferenceSource: Source[Int] = Source(testInt)
    val integerLiteralSource: Source[Int] = Source(0)

    val integerAdditionSource: Source[Int] = Source(4 + 2)
    val integerSubtractionSource: Source[Int] = Source(4 - 2)
    val integerMultiplicationSource: Source[Int] = Source(4 * 2)
    val integerDivisionSource: Source[Int] = Source(4 / 2)

    val testString: String = "foo"
    val stringReferenceSource: Source[String] = Source(testString)
    val stringLiteralSource: Source[String] = Source("abc")

    val testBoolean: Boolean = true
    val boolReferenceSource: Source[Boolean] = Source(testBoolean)

    val boolTrueSource: Source[Boolean] = Source(true)
    val boolFalseSource: Source[Boolean] = Source(false)
    val boolNotSource: Source[Boolean] = Source(!false)
    val boolAndSource1: Source[Boolean] = Source(true && true)
    val boolAndSource2: Source[Boolean] = Source(true && false)
    val boolOrSource1: Source[Boolean] = Source(true || false)
    val boolOrSource2: Source[Boolean] = Source(false || false)

    val boolLTSource: Source[Boolean] = Source(1 < 2)
    val boolGTSource: Source[Boolean] = Source(1 > 2)
    val boolLESource: Source[Boolean] = Source(1 <= 2)
    val boolGESource: Source[Boolean] = Source(1 >= 2)
    val boolEQSource: Source[Boolean] = Source(1 == 2)
    val boolNESource: Source[Boolean] = Source(1 != 2)

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