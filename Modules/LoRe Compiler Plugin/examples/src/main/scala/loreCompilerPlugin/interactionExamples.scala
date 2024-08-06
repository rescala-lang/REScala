package loreCompilerPlugin

import lore.dsl.*
import reactives.default.{Signal as Derived, Var as Source}

import scala.collection.immutable.Map

object interactionExamplesObject:
  def interactionExamplesFunction(): Unit =
    // ========= Misc tests =========

    // LHS "x => y" format for arrow functions doesn't work, but FunctionN does
    val arrowFun: Function2[Int, String, Int] = (x: Int, y: String) => x * 2

    val emptyList: List[Int] = List()
    val stringList: List[String] = List("cool value", "dope value")
    val derivedSourceList: Derived[Source[List[String]]] = Derived { Source(List("cool value", "dope value")) }

    // LHS tuples don't work, and Map syntax "x -> y" also doesn't work, but tuples do (both Tuple2 and syntactic sugar)
    val emptyMap: Map[Int, String] = Map()
    val stringMap: Map[Int, Tuple2[String, String]] = Map((0, ("cool", "value")), (1, Tuple2("dope", "value")))
    val derivedSourceMap: Derived[Source[Map[Int, String]]] = Derived { Source(Map((0, "cool value"), (1, "dope value"))) }

    // ========= Interaction tests =========

    val addAppointment1: UnboundInteraction[Tuple1[Int], Int] = Interaction[Int, Int]

    val addAppointment2: UnboundInteraction[Tuple1[Int], Int] = Interaction[Int, Int]
      .requires { (a: Int, b: Int) => a > b }

    val addAppointment3: InteractionWithExecutes[Tuple1[Int], Int] = Interaction[Int, Int]
      .requires { (a: Int, b: Int) => a > b }
      .executes { (a: Int, b: Int) => a }

    val addAppointment4: InteractionWithExecutes[Tuple1[Int], Int] = Interaction[Int, Int]
      .requires { (a: Int, b: Int) => a > b }
      .executes { (a: Int, b: Int) => a }
      .ensures { (a: Int, b: Int) => a > b }

    val integerSource: Source[Int] = Source(1)
    val integerDerived: Derived[Int] = Derived { integerSource.value + integerSource.value }

    val integerInteraction2 = Interaction[Int, Int]
      .requires { (curr: Int, _) => curr < 20 }
      .modifies { integerSource }
      .executes { (curr: Int, _) => curr + 10 }
  end interactionExamplesFunction
end interactionExamplesObject
