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



class except_EventTest extends tests.rescala.RETests {



  allEngines("handlerOf_except_IsExecutedIfBasicEventFires"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ((x: Int) => { test += 1 })

    e1(10)
    assert(test == 1)

  }

  allEngines("handlerOf_except_IgnoresTheSecondEventIfFires"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ((x: Int) => { test += 1 })

    e2(10)
    assert(test == 0)

  }

  allEngines("handlerOf_except_IsExecutedOnlyIfFirstEventFiresAndNotTheSecond"){ engine => import engine._

    var test = 0

    var cond = false
    val e1 = Evt[Int]
    val e2 = e1 map ((x: Int) => x * 2)
    val e3 = e1 filter (_ => cond)
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ((x: Int) => { test += 1 })


    e1(10)
    assert(test == 1)

    cond = true
    e1(10)
    assert(test == 1)

    cond = false
    e1(10)
    assert(test == 2)

  }


  allEngines("handlerOf_except_GetsTheCorrectValue"){ engine => import engine._

    var value = 0

    var cond = false
    val e1 = Evt[Int]
    val e2 = e1 map ((x: Int) => x)
    val e3 = (e1 map ((x: Int) => x * 2)) filter (_ => cond)
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ((x: Int) => { value = x })


    e1(10)
    assert(value == 10)

    cond = true
    e1(11)
    assert(value == 10)

    cond = false
    e1(12)
    assert(value == 12)

  }

}
