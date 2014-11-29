package tests.rescala.events


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Evt
import rescala.propagation.Engines
import Engines.default


class EventTest extends AssertionsForJUnit with MockitoSugar {

  @Test def handlersAreExecuted() = {
    var test = 0
    val e1 = new Evt[Int]()
    e1 += ((x: Int) => { test += 1 })
    e1(10)
    e1(10)
    assert(test == 2)
  }

  @Test def eventHandlersCanBeRemoved() = {
    var test = 0
    val e1 = new Evt[Int]()
    val f = (x: Int) => { test += 1 }
    val o = e1 += f
    e1(10)
    e1(10)
    o.remove()
    e1(10)
    assert(test == 2)
  }

  @Test def correctValueIsReceived() = {
    var test = 0
    val e1 = new Evt[Int]()
    e1 += ((x: Int) => { test += x })
    e1(10)
    assert(test == 10)
  }

  @Test def eventsWithoutParamsIsCalled() = {
    var test = 0
    val e1 = new Evt[Unit]()
    e1 += (_ => { test += 1 })
    e1(())
    assert(test == 1)
  }


  @Test def functionIsCalled() = {
    var test = 0

    def f(x: Int): Unit = { test += 1 }

    val e1 = new Evt[Int]()
    e1 += f

    e1(10)
    e1(10)
    assert(test == 2)
  }


  @Test def eventsWithMethodHandlersWithParameter() = {

    var test = 0
    val e = new Evt[Int]
    def m1(x: Int) = { test += 1 }

    e += m1
    e(10)
    e(10)
    assert(test == 2)

  }

}
