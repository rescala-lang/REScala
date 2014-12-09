package tests.rescala

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.turns.{Turn, Engines, Ticket}
import Engines.default
import rescala.{Signal, Signals, Var}

import scala.language.implicitConversions

class LightImplicitSyntaxTest extends AssertionsForJUnit {

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
