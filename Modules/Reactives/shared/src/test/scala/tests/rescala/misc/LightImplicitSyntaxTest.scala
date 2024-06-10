package tests.rescala.misc

import reactives.core.{CreationTicket, DynamicTicket}
import reactives.SelectedScheduler.candidate.State as BundleState
import tests.rescala.testtools.RETests

class LightImplicitSyntaxTest extends RETests {
  multiEngined { engine =>
    import engine.*

    test("experiment With Implicit Syntax") {

      implicit def getSignalValueDynamic[T](s: Signal[T])(using ticket: DynamicTicket[BundleState]): T =
        ticket.depend(s)

      def Signal[T](f: DynamicTicket[BundleState] ?=> T)(using maybe: CreationTicket[BundleState]): Signal[T] =
        engine.Signal.dynamic()(f(using _))

      val price    = Var(3)
      val tax      = price.map { p => p / 3 }
      val quantity = Var(1)
      val total = Signal {
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
