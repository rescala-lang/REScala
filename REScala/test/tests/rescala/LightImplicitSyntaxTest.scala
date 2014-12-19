package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import rescala.turns.{Engines, Engine, Ticket, Turn}
import rescala.{Signal, Signals, Var}

import scala.language.implicitConversions
object LightImplicitSyntaxTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class LightImplicitSyntaxTest(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine

  @Test def experimentWithImplicitSyntax(): Unit = {
    implicit def getSignalValueDynamic[T](s: Signal[T])(implicit turn: Turn): T = s.apply(turn)
    def Signal[T](f: Turn => T)(implicit maybe: Ticket): Signal[T] = Signals.dynamic()(f)

    val price = Var(3)
    val tax = price.map { p => p / 3 }
    val quantity = Var(1)
    val total = Signal { implicit t =>
      quantity * (price + tax)
    }

    assert(total.now === 4)
    price.set(6)
    assert(total.now === 8)
    quantity.set(2)
    assert(total.now === 16)

  }

}
