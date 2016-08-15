package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn


object MacroEventTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class MacroEventTestSuite[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit  {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Event, Evt, Signal}

  @Test def useEventsInSignalExpression(): Unit = {
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val res = Signal { List(e1(), e2()).flatten.sum }

    assert(res.now === 0)
    e1(10)
    assert(res.now === 10)
    e2(11)
    assert(res.now === 11)
    implicitEngine.plan(e1, e2) { t =>
      e1.admit(10)(t)
      e2.admit(10)(t)
    }
    assert(res.now === 20)

  }

  @Test def useEventExpression(): Unit = {
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val event = Event { Some(List(e1(), e2()).flatten) }
    val res = event.latest(Nil)

    assert(res.now === Nil)
    e1(9)
    assert(res.now === List(9))
    e2(10)
    assert(res.now === List(10))
    implicitEngine.plan(e1, e2) { t =>
      e1.admit(11)(t)
      e2.admit(12)(t)
    }
    assert(res.now === List(11, 12))

  }

}
