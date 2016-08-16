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




class dropParam_EventTest extends RETests {



  allEngines("handlerOf_dropParam_IsExecuted"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e1_drop: Event[Unit] = e1.dropParam
    e1_drop += ((x) => { test += 1; })

    e1(10)
    e1(10)
    assert(test == 2)
  }

}
