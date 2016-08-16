package tests.rescala


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn




class DynamicEventTestSuite extends RETests {




  allEngines("simple"){ engine => import engine._
    val ev1 = Evt[Int]
    val v1 = Var(8)
    val snapshotEvent = dynamicE() { t =>
      ev1(t).map(i => i + v1(t))
    }

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1(10)
    assert(res.now === 18)
    v1.set(7)
    assert(res.now === 18)
    ev1(10)
    assert(res.now === 17)



  }

}
