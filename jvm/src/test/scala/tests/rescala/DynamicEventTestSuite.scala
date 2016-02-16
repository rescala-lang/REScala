package tests.rescala


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn

object DynamicEventTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class DynamicEventTestSuite[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine

  import implicitEngine.{Evt, Var, dynamicE}

  @Test def simple(): Unit = {
    val ev1 = Evt[Int]()
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
