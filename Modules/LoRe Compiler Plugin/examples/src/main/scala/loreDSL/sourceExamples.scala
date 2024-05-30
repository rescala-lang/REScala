package loreDSL

import lore.dsl.*
import reactives.default.Var as Source

object sourceExamples:
  @main def main(): Unit =
    // ========= Non-Source Values to use in below tests =========
    val testIntLiteral: Int = 1
    val testIntExpression1: Int = 1 + 2
    val testIntExpression2: Int = 2 * 6 - 3
    val testIntExpression3: Int = (2 * 6) - 3
    val testIntExpression4: Int = 2 * (6 - 3)
    val testIntReference: Int = testIntLiteral

    val testStringLiteral: String = "foo"
    val testStringReference: String = testStringLiteral

    val testBooleanLiteral: Boolean = true
    val testBooleanExpression1: Boolean = true && false
    val testBooleanExpression2: Boolean = 2 > 1 || 1 + 1 == 2 && false
    val testBooleanExpression3: Boolean = (2 > 1 || 1 + 1 == 2) && false
    val testBooleanExpression4: Boolean = 2 > 1 || (1 + 1 == 2 && false)
    val testBooleanReference: Boolean = testBooleanLiteral

    // ========= Source tests =========

    // Simple integer values
    val integerLiteralSource: Source[Int] = Source(1)
    val integerReferenceSource: Source[Int] = Source(testIntLiteral)

    // Binary integer operators
    val integerLiteralAdditionSource: Source[Int] = Source(4 + 2)
    val integerLiteralSubtractionSource: Source[Int] = Source(4 - 2)
    val integerLiteralMultiplicationSource: Source[Int] = Source(4 * 2)
    val integerLiteralDivisionSource: Source[Int] = Source(4 / 2)

    // Binary integer operators with references
    val integerReferenceBinaryOpSource1: Source[Int] = Source(testIntLiteral + testIntLiteral)
    val integerReferenceBinaryOpSource2: Source[Int] = Source(4 - testIntLiteral)
    val integerReferenceBinaryOpSource3: Source[Int] = Source(testIntLiteral * 2)

    // Simple string values
    val stringLiteralSource: Source[String] = Source("bar")
    val stringReferenceSource: Source[String] = Source(testStringLiteral)

    // Simple boolean values
    val boolTrueSource: Source[Boolean] = Source(true)
    val boolFalseSource: Source[Boolean] = Source(false)
    val boolReferenceSource: Source[Boolean] = Source(testBooleanLiteral)

    // Unary boolean operators
    val boolNotSource: Source[Boolean] = Source(!true)
    val boolNotReferenceSource: Source[Boolean] = Source(!testBooleanLiteral)

    // Binary boolean operators
    val boolAndSource1: Source[Boolean] = Source(true && true)
    val boolAndSource2: Source[Boolean] = Source(true && false)
    val boolOrSource1: Source[Boolean] = Source(true || false)
    val boolOrSource2: Source[Boolean] = Source(false || false)

    // Binary boolean operators with references
    val boolReferenceBinaryOpSource1: Source[Boolean] = Source(testBooleanLiteral && testBooleanLiteral)
    val boolReferenceBinaryOpSource2: Source[Boolean] = Source(false || testBooleanLiteral)
    val boolReferenceBinaryOpSource3: Source[Boolean] = Source(testBooleanLiteral && true)

    // Binary boolean operators with numerical values and boolean output
    val boolLTSource: Source[Boolean] = Source(1 < 2)
    val boolGTSource: Source[Boolean] = Source(1 > 2)
    val boolLESource: Source[Boolean] = Source(1 <= 2)
    val boolGESource: Source[Boolean] = Source(1 >= 2)
    val boolEQSource: Source[Boolean] = Source(1 == 2)
    val boolNESource: Source[Boolean] = Source(1 != 2)

    // Binary boolean operators with numerical values and boolean output on references
    val boolReferenceNumeralBinaryOpSource1: Source[Boolean] = Source(testIntLiteral >= testIntLiteral + 1)
    val boolReferenceNumeralBinaryOpSource2: Source[Boolean] = Source(1 != testIntLiteral)
    val boolReferenceNumeralBinaryOpSource3: Source[Boolean] = Source(testIntLiteral <= 2)

      // ========= TODO: Derived and Interactions (in separate example files) =========

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
end sourceExamples
