package tests.rescala

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.turns.Engines.default
import rescala.turns.{Engine, Ticket, Turn}

class TicketTest extends AssertionsForJUnit with MockitoSugar {

  /* this test uses some shady newTurn(identity) to get the turn object out of the transaction
   * you should not do this. */

  @Test def noneDynamicNoImplicit(): Unit = {
    assert(implicitly[Ticket].self === Right(implicitly[Engine]))
  }

  @Test def someDynamicNoImplicit(): Unit = implicitly[Engine].plan { (dynamicTurn: Turn) =>
    assert(implicitly[Ticket].self === Right(implicitly[Engine]))
    assert(implicitly[Ticket].apply(identity) === dynamicTurn)
  }

  @Test def noneDynamicSomeImplicit(): Unit = {
    implicit val implicitTurn: Turn = implicitly[Engine].plan(identity)
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  @Test def someDynamicSomeImplicit(): Unit = implicitly[Engine].plan { (dynamicTurn: Turn) =>
    implicit val implicitTurn: Turn = implicitly[Engine].plan(identity)
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  @Test def implicitInClosures(): Unit = {
    val fac = implicitly[Engine]
    val closureDefinition = fac.plan(identity)
    val closure = {
      implicit def it: Turn = closureDefinition
      () => implicitly[Ticket]
    }
    fac.plan { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  @Test def dynamicInClosures(): Unit = {
    val fac = implicitly[Engine]
    val closure = {
      fac.plan { t =>
        () => implicitly[Ticket]
      }
    }
    fac.plan { dynamic =>
      assert(closure().self === Right(fac))
      assert(closure().apply(identity) === dynamic)
    }
  }

}
