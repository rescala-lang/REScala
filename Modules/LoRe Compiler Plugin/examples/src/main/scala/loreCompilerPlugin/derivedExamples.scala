package loreCompilerPlugin

import lore.dsl.*
import reactives.default.{Var as Source, Signal as Derived}

object derivedExamples:
  def derivedExamples(): Unit =
    // ========= Plain values and sources to use in below tests =========
    val integerLiteral: Int = 1
    val stringLiteral: String = "foo"
    val booleanLiteral: Boolean = true

    val integerLiteralSource1: Source[Int] = Source(1)
    val integerLiteralSource2: Source[Int] = Source(2)
    val stringLiteralSource: Source[String] = Source("bar")
    val boolLiteralSource: Source[Boolean] = Source(false)

    // ========= Derived tests =========

    // These will have constant values, as they depend on values that are not mutable
    val integerLiteralDerived: Derived[Int] = Derived { 1 + 2 }
    val integerReferenceDerived1: Derived[Int] = Derived { integerLiteral + 2 }
    val integerReferenceDerived2: Derived[Int] = Derived { 1 + integerLiteral }

    // Single and double dependencies on integer sources
    val integerSourceDerived1: Derived[Int] = Derived { integerLiteralSource1.value + 2 }
    val integerSourceDerived2: Derived[Int] = Derived { 1 + integerLiteralSource2.value }
    val integerSourceDerived3: Derived[Int] = Derived { integerLiteralSource1.value + integerLiteralSource2.value }

//   ========= TODO: Interactions (in separate example file) =========
//      val integerInteraction = Interaction[Int, Int]
//        .requires { (curr, _) => curr < 20 }
//        .modifies { integerSource }
//        .executes { (curr, _) => curr + 10 }
//
//      println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")
//
//      integerInteraction(0)
//      println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")
//
//      integerInteraction(0)
//      println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")
//
//      integerInteraction(0)
//      println(s"integerSource: ${integerSource.now}, integerDerived: ${integerDerived.now}")
  end derivedExamples
end derivedExamples
