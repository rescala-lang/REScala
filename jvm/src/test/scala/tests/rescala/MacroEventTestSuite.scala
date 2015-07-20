package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Infiltrator.getLevel
import rescala.Signals
import rescala.graph.Spores
import rescala.turns.{Engine, Turn}


object MacroEventTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class MacroEventTestSuite[S <: Spores](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Evt, Var, Signal, Event}

  @Test def useEventsInSignalExpression(): Unit = {
    val e1 = Evt[Int]()
    val e2 = Evt[Int]()
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

}
