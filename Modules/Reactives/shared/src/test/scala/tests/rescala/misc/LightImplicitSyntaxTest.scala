package tests.rescala.misc

import reactives.core.{CreationTicket, DynamicTicket}
import tests.rescala.testtools.RETests
import reactives.default.global.State as BundleState

class LightImplicitSyntaxTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("experiment With Implicit Syntax") {

      implicit def getSignalValueDynamic[T](s: Signal[T])(implicit ticket: DynamicTicket[BundleState]): T =
        ticket.depend(s)
      def Signal[T](f: DynamicTicket[BundleState] => T)(implicit maybe: CreationTicket[BundleState]): Signal[T] =
        engine.Signal.dynamic()(f)

      val price    = Var(3)
      val tax      = price.map { p => p / 3 }
      val quantity = Var(1)
      val total = Signal { implicit t =>
        quantity * (price + tax)
      }

      assertEquals(total.readValueOnce, 4)
      price.set(6)
      assertEquals(total.readValueOnce, 8)
      quantity.set(2)
      assertEquals(total.readValueOnce, 16)

    }

  }
}
