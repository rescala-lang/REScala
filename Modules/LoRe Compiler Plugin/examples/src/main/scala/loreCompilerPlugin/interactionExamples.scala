package loreCompilerPlugin

import lore.dsl.*
import reactives.default.{Signal as Derived, Var as Source}

import scala.collection.immutable.Map

object interactionExamplesObject:
  def interactionExamplesFunction(): Unit =
    // ========= Misc tests =========
    val arrowFun = (x: Int, y: String) => x * 2

    val emptyList: List[Int]                             = List()
    val stringList: List[String]                         = List("cool value", "dope value")
    val derivedSourceList: Derived[Source[List[String]]] = Derived { Source(List("cool value", "dope value")) }

    // Map syntax "x -> y" in RHS currently does not work
    val emptyMap: Map[Int, String]            = Map()
    val stringMap: Map[Int, (String, String)] = Map((0, ("cool", "value")), (1, Tuple2("dope", "value")))
    val derivedSourceMap: Derived[Source[Map[Int, String]]] = Derived {
      Source(Map((0, "cool value"), (1, "dope value")))
    }

    // ========= Interaction tests =========

    val integerInteraction1 = Interaction[Int, Int]

    val integerInteraction2 = Interaction[Int, Int]
      .requires((a: Int, b: Int) => a > b)

    val integerInteraction3 = Interaction[Int, Int]
      .requires ((a: Int, b: Int) => a > b)
      .executes ((a: Int, b: Int) => a )
      .requires ((a: Int, b: Int) => a > b + 1)

    val integerInteraction4 = Interaction[Int, Int]
      .requires ( (a: Int, b: Int) => a > b )
      .executes ( (a: Int, b: Int) => a )
      .ensures ( (a: Int, b: Int) => a > b )

    val integerSource: Source[Int]   = Source(1)
    val integerDerived: Derived[Int] = Derived { integerSource.value + integerSource.value }

    val integerInteraction5 = Interaction[Int, Int]
      .requires ( (curr: Int, _) => curr < 3 )
      .modifies ( integerSource )
      .executes ( (curr: Int, _) => curr + 1 )
  end interactionExamplesFunction
end interactionExamplesObject
