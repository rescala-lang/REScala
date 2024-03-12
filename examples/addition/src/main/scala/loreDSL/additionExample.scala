package loreDSL

import lore.DSL.*

object additionExample:
  @main def main(): Unit =
    // ========= Non-Source Values to use in below tests =========
    val testIntReference1: Int = 0
    val testIntReference2: Int = 1 + 2
    val testIntReference3: Int = testIntReference1

    val testIntBinaryOpReference1: Int = 4
    val testIntBinaryOpReference2: Int = 2

    val testStringReference: String = "foo"

    val testBooleanReference1: Boolean = true
    val testBooleanReference2: Boolean = true && false
    val testBooleanReference3: Boolean = testBooleanReference1

    val testBooleanBinaryOpReference1: Boolean = true
    val testBooleanBinaryOpReference2: Boolean = false

    val testIntBinaryBoolOpReference1: Int = 1
    val testIntBinaryBoolOpReference2: Int = 2

    // ========= Source tests =========

    // Simple integer values
    val integerLiteralSource: Source[Int] = Source(0)
    val integerReferenceSource: Source[Int] = Source(testIntReference1)

    // Binary integer operators
    val integerLiteralAdditionSource: Source[Int] = Source(4 + 2)
    val integerLiteralSubtractionSource: Source[Int] = Source(4 - 2)
    val integerLiteralMultiplicationSource: Source[Int] = Source(4 * 2)
    val integerLiteralDivisionSource: Source[Int] = Source(4 / 2)

    // Binary integer operators with references
    val integerReferenceAdditionSource1: Source[Int] = Source(testIntBinaryOpReference1 + testIntBinaryOpReference2)
    val integerReferenceAdditionSource2: Source[Int] = Source(4 + testIntBinaryOpReference2)
    val integerReferenceAdditionSource3: Source[Int] = Source(testIntBinaryOpReference1 + 2)

    // Simple string values
    val stringLiteralSource: Source[String] = Source("abc")
    val stringReferenceSource: Source[String] = Source(testStringReference)

    // Simple boolean values
    val boolTrueSource: Source[Boolean] = Source(true)
    val boolFalseSource: Source[Boolean] = Source(false)
    val boolReferenceSource: Source[Boolean] = Source(testBooleanReference1)

    // Unary boolean operators
    val boolNotSource: Source[Boolean] = Source(!false)
    val boolNotReferenceSource: Source[Boolean] = Source(!testBooleanReference1)

    // Binary boolean operators
    val boolAndSource1: Source[Boolean] = Source(true && true)
    val boolAndSource2: Source[Boolean] = Source(true && false)
    val boolOrSource1: Source[Boolean] = Source(true || false)
    val boolOrSource2: Source[Boolean] = Source(false || false)

    // Binary boolean operators with references
    val boolReferenceAndSource1: Source[Boolean] = Source(testBooleanBinaryOpReference1 && testBooleanBinaryOpReference2)
    val boolReferenceAndSource2: Source[Boolean] = Source(true && testBooleanBinaryOpReference2)
    val boolReferenceAndSource3: Source[Boolean] = Source(testBooleanBinaryOpReference1 && false)

    // Binary boolean operators with numerical values and boolean output
    val boolLTSource: Source[Boolean] = Source(1 < 2)
    val boolGTSource: Source[Boolean] = Source(1 > 2)
    val boolLESource: Source[Boolean] = Source(1 <= 2)
    val boolGESource: Source[Boolean] = Source(1 >= 2)
    val boolEQSource: Source[Boolean] = Source(1 == 2)
    val boolNESource: Source[Boolean] = Source(1 != 2)

    // Binary boolean operators with numerical values and boolean output on references
    val boolReferenceNESource1: Source[Boolean] = Source(testIntBinaryBoolOpReference1 != testIntBinaryBoolOpReference2)
    val boolReferenceNESource2: Source[Boolean] = Source(1 != testIntBinaryBoolOpReference2)
    val boolReferenceNESource3: Source[Boolean] = Source(testIntBinaryBoolOpReference1 != 2)

      // ========= TODO: Derived and Interactions =========

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