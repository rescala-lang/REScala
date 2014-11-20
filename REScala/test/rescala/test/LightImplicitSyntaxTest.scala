package rescala.test

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.propagation.{TurnFactory, Turn, MaybeTurn}
import rescala.signals.{Signals, Var, Signal}
import scala.language.implicitConversions

class LightImplicitSyntaxTest extends AssertionsForJUnit {

  @Test def experimentWithImplicitSyntax(): Unit = {
    implicit def getSignalValueDynamic[T](s: Signal[T])(implicit turn: Turn): T = s.apply(turn)
    def Signal[T](f: Turn => T)(implicit maybe: MaybeTurn): Signal[T] = Signals.dynamic()(f)

    val price = Var(3)
    val tax = price.map { p => p / 3 }
    val quantity = Var(1)
    val total = Signal { implicit t =>
      quantity * (price + tax)
    }

    assert(total.get === 4)
    price.set(6)
    assert(total.get === 8)
    quantity.set(2)
    assert(total.get === 16)

  }

}
