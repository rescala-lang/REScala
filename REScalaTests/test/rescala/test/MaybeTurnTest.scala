package rescala.test

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import rescala.propagation._

class MaybeTurnTest extends AssertionsForJUnit with MockitoSugar {

  @Test def noneDynamicNoImplicit() = Turn.currentTurn.withValue(None) {
    assert(implicitly[MaybeTurn] === MaybeTurn(None))
  }

  @Test def someDynamicNoImplicit() = Turn.newTurn { (dynamicTurn: Turn) =>
    assert(implicitly[MaybeTurn] === MaybeTurn(Some(dynamicTurn)))
  }

  @Test def noneDynamicSomeImplicit() = Turn.currentTurn.withValue(None) {
    implicit val implicitTurn: Turn = new Turn
    assert(implicitly[MaybeTurn] === MaybeTurn(Some(implicitTurn)))
  }

  @Test def someDynamicSomeImplicit() = Turn.newTurn { (dynamicTurn: Turn) =>
    implicit val implicitTurn: Turn = new Turn
    assert(implicitly[MaybeTurn] === MaybeTurn(Some(implicitTurn)))
  }

  @Test def implicitInClosures() = {
    val closureDefinition = new Turn
    val closure = {
      implicit def it: Turn = closureDefinition
      (x: Unit) => implicitly[MaybeTurn]
    }
    Turn.newTurn { dynamic =>
      assert(closure(Unit) === MaybeTurn(Some(closureDefinition)))
    }
  }

  @Test def dynamicInClosures() = {
    val closureDefinition = new Turn
    val closure = {
      Turn.currentTurn.withValue(Some(closureDefinition)) {
        (x: Unit) => implicitly[MaybeTurn]
      }
    }
    Turn.newTurn { dynamic =>
      assert(closure(Unit) === MaybeTurn(Some(dynamic)))
    }
  }

}
