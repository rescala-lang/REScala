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

class EventCreationTest extends RETests {





  allEngines("addEventAfter"){ engine => import engine._
    var res = 0
    val e0 = Evt[Int]
    val e1 = e0.map(identity)
    e1.map(_ => e0.map {_ + 1}.observe {res = _})
    e0(10)

    assert(res === 11)

  }

}
