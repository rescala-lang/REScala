package lore.dsl

import lore.dsl.{Ex, *}
import reactives.default.*

class InteractionTest extends munit.FunSuite {

  test("Test Interaction with single source") {
    val v = Var(0)
    val e = Evt[Int]()

    var t = 0

    val add10 =
      Interaction[Int, Int]
        .requires[Int]((t: Int, _) => t < 20)
        .modifies(v)
        .executes((t: Int, _) => t + 10)
        .ensures[Int]((t, _) => t < 15)
        .actsOn(e)

    v.observe { it =>
      t = it
    }

    e.fire(10)

    assertEquals(t, 10)

    intercept[IllegalStateException] {
      e.fire(10)
    }
  }

}
