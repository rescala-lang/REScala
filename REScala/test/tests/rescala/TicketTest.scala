package tests.rescala

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.propagation.{Ticket, Engine, Engines}
import Engines.default
import rescala.propagation.turns.Turn

class TicketTest extends AssertionsForJUnit with MockitoSugar {

  /* this test uses some shady newTurn(identity) to get the turn object out of the transaction
   * you should not do this. */

  @Test def noneDynamicNoImplicit(): Unit = {
    assert(implicitly[Ticket].self === Right(implicitly[Engine]))
  }

  @Test def someDynamicNoImplicit(): Unit = implicitly[Engine].startNew { (dynamicTurn: Turn) =>
    assert(implicitly[Ticket].self === Right(implicitly[Engine]))
    assert(implicitly[Ticket].apply(identity) === dynamicTurn)
  }

  @Test def noneDynamicSomeImplicit(): Unit = {
    implicit val implicitTurn: Turn = implicitly[Engine].startNew(identity)
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  @Test def someDynamicSomeImplicit(): Unit = implicitly[Engine].startNew { (dynamicTurn: Turn) =>
    implicit val implicitTurn: Turn = implicitly[Engine].startNew(identity)
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  @Test def implicitInClosures(): Unit = {
    val fac = implicitly[Engine]
    val closureDefinition = fac.startNew(identity)
    val closure = {
      implicit def it: Turn = closureDefinition
      () => implicitly[Ticket]
    }
    fac.startNew { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  @Test def dynamicInClosures(): Unit = {
    val fac = implicitly[Engine]
    val closure = {
      fac.startNew { t =>
        () => implicitly[Ticket]
      }
    }
    fac.startNew { dynamic =>
      assert(closure().self === Right(fac))
      assert(closure().apply(identity) === dynamic)
    }
  }

}
