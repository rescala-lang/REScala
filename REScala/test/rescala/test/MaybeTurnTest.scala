package rescala.test

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Implicits.default
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.{MaybeTurn, TurnFactory}

class MaybeTurnTest extends AssertionsForJUnit with MockitoSugar {

  /* this test uses some shady newTurn(identity) to get the turn object out of the transaction
   * you should not do this. */

  @Test def noneDynamicNoImplicit(): Unit = {
    assert(implicitly[MaybeTurn].self === Right(implicitly[TurnFactory]))
  }

  @Test def someDynamicNoImplicit(): Unit = implicitly[TurnFactory].newTurn { (dynamicTurn: Turn) =>
    assert(implicitly[MaybeTurn].self === Right(implicitly[TurnFactory]))
    assert(implicitly[MaybeTurn].apply(identity) === dynamicTurn)
  }

  @Test def noneDynamicSomeImplicit(): Unit = {
    implicit val implicitTurn: Turn = implicitly[TurnFactory].newTurn(identity)
    assert(implicitly[MaybeTurn].self === Left(implicitTurn))
    assert(implicitly[MaybeTurn].apply(identity) === implicitTurn)
  }

  @Test def someDynamicSomeImplicit(): Unit = implicitly[TurnFactory].newTurn { (dynamicTurn: Turn) =>
    implicit val implicitTurn: Turn = implicitly[TurnFactory].newTurn(identity)
    assert(implicitly[MaybeTurn].self === Left(implicitTurn))
    assert(implicitly[MaybeTurn].apply(identity) === implicitTurn)
  }

  @Test def implicitInClosures(): Unit = {
    val fac = implicitly[TurnFactory]
    val closureDefinition = fac.newTurn(identity)
    val closure = {
      implicit def it: Turn = closureDefinition
      () => implicitly[MaybeTurn]
    }
    fac.newTurn { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  @Test def dynamicInClosures(): Unit = {
    val fac = implicitly[TurnFactory]
    val closure = {
      fac.newTurn { t =>
        () => implicitly[MaybeTurn]
      }
    }
    fac.newTurn { dynamic =>
      assert(closure().self === Right(fac))
      assert(closure().apply(identity) === dynamic)
    }
  }

}
