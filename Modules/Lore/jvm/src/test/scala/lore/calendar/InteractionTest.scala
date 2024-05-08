package lore.calendar

import lore.dsl.*
import lore.dsl.Ex
import reactives.default.*

class InteractionTest extends munit.FunSuite {

  test("Test Interaction with single source") {
    val v = Var(0)
    val e = Evt[Int]()

    val add10 =
      Interaction[Int, Int]
        .requires[Int]((t: Int, _) => t < 20)
        .modifies(v)
        .executes((t: Int, _) => t + 10)
        .ensures[Int]((t, _) => t < 15)
        .actsOn(e)

    add10(0)
    
  }

}
