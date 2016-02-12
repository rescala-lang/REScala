package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Spores
import rescala.pipelining.PipelineEngine
import rescala.turns.{Engine, Ticket, Turn}

object TicketTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class TicketTest[S <: Spores](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine

  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn(implicit engine: Engine[S, Turn[S]]): Turn[S] = engine.plan()(identity)

  @Test def noneDynamicNoImplicit(): Unit = {
    assert(implicitly[Ticket[S]].self === Right(implicitly[Engine[S, Turn[S]]]))
  }

  @Test def someDynamicNoImplicit(): Unit = implicitly[Engine[S, Turn[S]]].plan() { (dynamicTurn: Turn[S]) =>
    assert(implicitly[Ticket[S]].self === Right(implicitly[Engine[S, Turn[S]]]))
    assert(implicitly[Ticket[S]].apply(identity) === dynamicTurn)
  }

  @Test def noneDynamicSomeImplicit(): Unit = {
    implicit val implicitTurn: Turn[S] = getTurn
    assert(implicitly[Ticket[S]].self === Left(implicitTurn))
    assert(implicitly[Ticket[S]].apply(identity) === implicitTurn)
  }

  // Cannot run a turn inside a turn with pipelining
  @Test def someDynamicSomeImplicit(): Unit =
    if (engine == PipelineEngine) {
      throw new IllegalStateException("pipeline engine cannot run a turn inside a turn")
    }
    else {
      implicitly[Engine[S, Turn[S]]].plan() { (dynamicTurn: Turn[S]) =>
        implicit val implicitTurn: Turn[S] = getTurn
        assert(implicitly[Ticket[S]].self === Left(implicitTurn))
        assert(implicitly[Ticket[S]].apply(identity) === implicitTurn)
      }
    }

  @Test def implicitInClosures(): Unit = {
    val fac = implicitly[Engine[S, Turn[S]]]
    val closureDefinition = getTurn(fac)
    val closure = {
      implicit def it: Turn[S] = closureDefinition
      () => implicitly[Ticket[S]]
    }
    fac.plan() { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  @Test def dynamicInClosures(): Unit = {
    val fac = implicitly[Engine[S, Turn[S]]]
    val closure = {
      fac.plan() { t =>
        () => implicitly[Ticket[S]]
      }
    }
    fac.plan() { dynamic =>
      assert(closure().self === Right(fac))
      assert(closure().apply(identity) === dynamic)
    }
  }

}
