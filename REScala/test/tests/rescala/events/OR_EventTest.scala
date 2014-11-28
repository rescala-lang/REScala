package tests.rescala.events


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Evt
import rescala.Implicits.default



class OR_EventTest extends AssertionsForJUnit with MockitoSugar {

  @Test def handlerOf_OR_IsExecutedIfAnyOfTheEventsFires() = {
    var test = 0
    val e1 = new Evt[Int]()
    val e2 = new Evt[Int]()
    val e1_OR_e2 = e1 || e2
    e1_OR_e2 += ((x: Int) => { test += 1 })

    e1(10)
    e2(10)
    assert(test == 2)

  }

  @Test def handlerOf_OR_IsExecutedOnlyOnce() = {

    var test = 0
    val e1 = new Evt[Int]()
    val e2 = e1 map ((x: Int) => x * 2)
    val e3 = e1 map ((x: Int) => x * 2)
    val e2_OR_e3 = e2 || e3
    e1 += ((x: Int) => { test += 1 })
    e2 += ((x: Int) => { test += 1 })
    e3 += ((x: Int) => { test += 1 })
    e2_OR_e3 += ((x: Int) => { test += 1 })

    e1(10)
    assert(test == 4)
  }

}
