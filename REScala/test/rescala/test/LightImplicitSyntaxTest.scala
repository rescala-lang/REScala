package rescala.test

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.propagation.Turn
import rescala.signals.{Signals, Var, Signal}
import scala.language.implicitConversions

class LightImplicitSyntaxTest extends AssertionsForJUnit {

  @Test def experimentWithImplicitSyntax(): Unit = {
    implicit def getSignalValueDynamic[T](s: Signal[T])(implicit turn: Turn): T = s.apply(turn)

    val v1 = Var(1)
    val s1 = Signals.dynamic(){ implicit t => v1 + v1}

    assert( s1.get === 2)

    v1.set(2)

    assert(s1.get === 4)

  }

}
