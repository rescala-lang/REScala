package loreDSL

import lore.DSL.*

object additionExample:
  @main def main(): Unit =
    // ========= Integer tests =========

    // Simple values
    val integerLiteralSource: Source[Int] = Source(0)
    val testIntReference: Int = 0
    val integerReferenceSource: Source[Int] = Source(testIntReference)

    // Binary operators
    val integerLiteralAdditionSource: Source[Int] = Source(4 + 2)
    val integerLiteralSubtractionSource: Source[Int] = Source(4 - 2)
    val integerLiteralMultiplicationSource: Source[Int] = Source(4 * 2)
    val integerLiteralDivisionSource: Source[Int] = Source(4 / 2)

    // Binary operators with references
    val testIntReference2: Int = 4
    val testIntReference3: Int = 2
    val integerReferenceAdditionSource1: Source[Int] = Source(testIntReference2 + testIntReference3)
    val integerReferenceAdditionSource2: Source[Int] = Source(4 + testIntReference3)
    val integerReferenceAdditionSource3: Source[Int] = Source(testIntReference2 + 2)

    // ========= String tests =========

    // Simple values
    val stringLiteralSource: Source[String] = Source("abc")
    val testString: String = "foo"
    val stringReferenceSource: Source[String] = Source(testString)

    // ========= Boolean tests =========

    // Simple values
    val boolTrueSource: Source[Boolean] = Source(true)
    val boolFalseSource: Source[Boolean] = Source(false)
    val testBoolean: Boolean = true
    val boolReferenceSource: Source[Boolean] = Source(testBoolean)

    // Binary operators
    val boolNotSource: Source[Boolean] = Source(!false)
    val boolAndSource1: Source[Boolean] = Source(true && true)
    val boolAndSource2: Source[Boolean] = Source(true && false)
    val boolOrSource1: Source[Boolean] = Source(true || false)
    val boolOrSource2: Source[Boolean] = Source(false || false)

    // Binary operators with references
    val testBoolean2: Boolean = true
    val testBoolean3: Boolean = false
    val boolReferenceAndSource1: Source[Boolean] = Source(testBoolean2 && testBoolean3)
    val boolReferenceAndSource2: Source[Boolean] = Source(true && testBoolean3)
    val boolReferenceAndSource3: Source[Boolean] = Source(testBoolean2 && false)

    // Binary operators with numerical values and boolean output
    val boolLTSource: Source[Boolean] = Source(1 < 2)
    val boolGTSource: Source[Boolean] = Source(1 > 2)
    val boolLESource: Source[Boolean] = Source(1 <= 2)
    val boolGESource: Source[Boolean] = Source(1 >= 2)
    val boolEQSource: Source[Boolean] = Source(1 == 2)
    val boolNESource: Source[Boolean] = Source(1 != 2)

    // Binary operators with numerical values and boolean output on references
    val testIntReference4: Int = 1
    val testIntReference5: Int = 2
    val boolReferenceNESource1: Source[Boolean] = Source(testIntReference4 != testIntReference5)
    val boolReferenceNESource2: Source[Boolean] = Source(1 != testIntReference5)
    val boolReferenceNESource3: Source[Boolean] = Source(testIntReference4 != 2)

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