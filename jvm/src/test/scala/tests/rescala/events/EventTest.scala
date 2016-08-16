package tests.rescala.events


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import tests.rescala.JUnitParameters

import tests.rescala.RETests



class EventTest extends RETests {



  allEngines("handlersAreExecuted"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    e1 += ((x: Int) => { test += 1 })
    e1(10)
    e1(10)
    assert(test == 2)
  }

  allEngines("eventHandlersCanBeRemoved"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val f = (x: Int) => { test += 1 }
    val o = e1 += f
    e1(10)
    e1(10)
    assert(test == 2)
    o.remove()
    e1(10)
    assert(test == 2)
  }

  allEngines("correctValueIsReceived"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    e1 += ((x: Int) => { test += x })
    e1(10)
    assert(test == 10)
  }

  allEngines("eventsWithoutParamsIsCalled"){ engine => import engine._
    var test = 0
    val e1 = Evt[Unit]
    e1 += (_ => { test += 1 })
    e1(())
    assert(test == 1)
  }


  allEngines("functionIsCalled"){ engine => import engine._
    var test = 0

    def f(x: Int): Unit = { test += 1 }

    val e1 = Evt[Int]
    e1 += f

    e1(10)
    e1(10)
    assert(test == 2)
  }


  allEngines("eventsWithMethodHandlersWithParameter"){ engine => import engine._

    var test = 0
    val e = Evt[Int]
    def m1(x: Int): Unit = { test += 1 }

    e += m1
    e(10)
    e(10)
    assert(test == 2)

  }

}
