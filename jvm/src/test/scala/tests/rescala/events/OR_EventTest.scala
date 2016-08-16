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




class OR_EventTest extends RETests {



  allEngines("handlerOf_OR_IsExecutedIfAnyOfTheEventsFires"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_OR_e2 = e1 || e2
    e1_OR_e2 += { _ => test += 1 }

    e1(10)
    e2(10)
    assert(test == 2)

  }

  allEngines("handlerOf_OR_IsExecutedOnlyOnce"){ engine => import engine._

    var test = 0
    val e1 = Evt[Int]
    val e2 = e1 map (_ * 2)
    val e3 = e1 map (_ * 2)
    val e2_OR_e3 = e2 || e3
    e1 += { _ => test += 1 }
    e2 += { _ => test += 1 }
    e3 += { _ => test += 1 }
    e2_OR_e3 += { _ => test += 1 }

    e1(10)
    assert(test == 4)
  }

}
