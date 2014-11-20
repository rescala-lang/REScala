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

    val v1 = Var(1)
    val s1 = Signal{ implicit t => v1 + v1}

    assert( s1.get === 2)

    v1.set(2)

    assert(s1.get === 4)

  }

}
