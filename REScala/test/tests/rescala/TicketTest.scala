package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.turns.{Engine, Ticket, Turn}
object TicketTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class TicketTest(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine

  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn(implicit engine: Engine[Turn]): Turn = engine.planned()(identity)
  
  @Test def noneDynamicNoImplicit(): Unit = {
    assert(implicitly[Ticket].self === Right(implicitly[Engine[Turn]]))
  }

  @Test def someDynamicNoImplicit(): Unit = implicitly[Engine[Turn]].planned() { (dynamicTurn: Turn) =>
    assert(implicitly[Ticket].self === Right(implicitly[Engine[Turn]]))
    assert(implicitly[Ticket].apply(identity) === dynamicTurn)
  }

  @Test def noneDynamicSomeImplicit(): Unit = {
    implicit val implicitTurn: Turn = getTurn
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  @Test def someDynamicSomeImplicit(): Unit = implicitly[Engine[Turn]].planned() { (dynamicTurn: Turn) =>
    implicit val implicitTurn: Turn = getTurn
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  @Test def implicitInClosures(): Unit = {
    val fac = implicitly[Engine[Turn]]
    val closureDefinition = getTurn(fac)
    val closure = {
      implicit def it: Turn = closureDefinition
      () => implicitly[Ticket]
    }
    fac.planned() { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  @Test def dynamicInClosures(): Unit = {
    val fac = implicitly[Engine[Turn]]
    val closure = {
      fac.planned() { t =>
        () => implicitly[Ticket]
      }
    }
    fac.planned() { dynamic =>
      assert(closure().self === Right(fac))
      assert(closure().apply(identity) === dynamic)
    }
  }

}
