package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Spores
import rescala.propagation.Turn
import rescala.engines.{Engine, Ticket}
import rescala.{Signal, Signals, Var}

import scala.language.implicitConversions

object LightImplicitSyntaxTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class LightImplicitSyntaxTest[S <: Spores](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Evt, Var, Signal, Event, dynamic}

  @Test def experimentWithImplicitSyntax(): Unit = {
    implicit def getSignalValueDynamic[T](s: Signal[T])(implicit turn: Turn[S]): T = s.apply(turn)
    def Signal[T](f: Turn[S] => T)(implicit maybe: Ticket[S]): Signal[T] = dynamic()(f)

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
