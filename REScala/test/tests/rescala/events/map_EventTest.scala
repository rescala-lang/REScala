package tests.rescala.events


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Evt
import rescala.turns.Engines.default


class map_EventTest extends AssertionsForJUnit with MockitoSugar {

  @Test def handlerOf_map_IsExecuted() = {
    var test = 0
    val e1 = new Evt[Int]()
    val e1_map = e1 map ((x: Int) => x * 2)
    e1_map += ((x: Int) => { test += 1 })

    e1(10)
    e1(10)
    assert(test == 2)
  }

  @Test def theFunctionPassedTo_map_isApplied() = {
    var test = 0
    val e1 = new Evt[Int]()
    val e1_map = e1 map ((x: Int) => x * 2)
    e1_map += ((x: Int) => { test = x })

    e1(10)
    e1(10)
    assert(test == 20)
  }

}
