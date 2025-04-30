package loreCompilerPlugin

import lore.dsl.*
import loreCompilerPlugin.annotation.LoReProgram
import reactives.default.Var as Source

object sourceExamplesObject:
  @LoReProgram
  def sourceExamplesFunction(): Unit = {
    // ========= Non-Source Values to use in below tests =========
    val integerLiteral: Int     = 1
    val integerExpression1: Int = 1 + 2
    val integerExpression2: Int = 2 * 6 - 3
    val integerExpression3: Int = (2 * 6) - 3
    val integerExpression4: Int = 2 * (6 - 3)
    val integerReference: Int   = integerLiteral

    val stringLiteral: String   = "foo"
    val stringReference: String = stringLiteral

    val booleanLiteral: Boolean     = true
    val booleanExpression1: Boolean = true && false
    val booleanExpression2: Boolean = 2 > 1 || 1 + 1 == 2 && false
    val booleanExpression3: Boolean = (2 > 1 || 1 + 1 == 2) && false
    val booleanExpression4: Boolean = 2 > 1 || (1 + 1 == 2 && false)
    val booleanReference: Boolean   = booleanLiteral

    // ========= Source tests =========

    // Simple integer values
    val integerLiteralSource: Source[Int]   = Source(1)
    val integerReferenceSource: Source[Int] = Source(integerLiteral)

    // Binary integer operators
    val integerLiteralAdditionSource: Source[Int]       = Source(4 + 2)
    val integerLiteralSubtractionSource: Source[Int]    = Source(4 - 2)
    val integerLiteralMultiplicationSource: Source[Int] = Source(4 * 2)
    val integerLiteralDivisionSource: Source[Int]       = Source(4 / 2)

    // Binary integer operators with references
    val integerReferenceBinaryOpSource1: Source[Int] = Source(integerLiteral + integerLiteral)
    val integerReferenceBinaryOpSource2: Source[Int] = Source(4 - integerLiteral)
    val integerReferenceBinaryOpSource3: Source[Int] = Source(integerLiteral * 2)

    // Simple string values
    val stringLiteralSource: Source[String]   = Source("bar")
    val stringReferenceSource: Source[String] = Source(stringLiteral)

    // Simple boolean values
    val boolTrueSource: Source[Boolean]      = Source(true)
    val boolFalseSource: Source[Boolean]     = Source(false)
    val boolReferenceSource: Source[Boolean] = Source(booleanLiteral)

    // Unary boolean operators
    val boolNotSource: Source[Boolean]          = Source(!true)
    val boolNotReferenceSource: Source[Boolean] = Source(!booleanLiteral)

    // Binary boolean operators
    val boolAndSource1: Source[Boolean] = Source(true && true)
    val boolAndSource2: Source[Boolean] = Source(true && false)
    val boolOrSource1: Source[Boolean]  = Source(true || false)
    val boolOrSource2: Source[Boolean]  = Source(false || false)

    // Binary boolean operators with references
    val boolReferenceBinaryOpSource1: Source[Boolean] = Source(booleanLiteral && booleanLiteral)
    val boolReferenceBinaryOpSource2: Source[Boolean] = Source(false || booleanLiteral)
    val boolReferenceBinaryOpSource3: Source[Boolean] = Source(booleanLiteral && true)

    // Binary boolean operators with numerical values and boolean output
    val boolLTSource: Source[Boolean] = Source(1 < 2)
    val boolGTSource: Source[Boolean] = Source(1 > 2)
    val boolLESource: Source[Boolean] = Source(1 <= 2)
    val boolGESource: Source[Boolean] = Source(1 >= 2)
    val boolEQSource: Source[Boolean] = Source(1 == 2)
    val boolNESource: Source[Boolean] = Source(1 != 2)

    // Binary boolean operators with numerical values and boolean output on references
    val boolReferenceNumeralBinaryOpSource1: Source[Boolean] = Source(integerLiteral >= integerLiteral + 1)
    val boolReferenceNumeralBinaryOpSource2: Source[Boolean] = Source(1 != integerLiteral)
    val boolReferenceNumeralBinaryOpSource3: Source[Boolean] = Source(integerLiteral <= 2)
  }
  end sourceExamplesFunction
end sourceExamplesObject
