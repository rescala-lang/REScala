package tests.rescala.misc

import tests.rescala.testtools.RETests

class LightImplicitSyntaxTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("experiment With Implicit Syntax") {

      implicit def getSignalValueDynamic[T](s: Signal[T])(implicit ticket: engine.DynamicTicket): T = ticket.depend(s)
      def Signal[T](f: DynamicTicket => T)(implicit maybe: CreationTicket): Signal[T] = Signals.dynamic()(f)

      val price    = Var(3)
      val tax      = price.map { p => p / 3 }
      val quantity = Var(1)
      val total = Signal { implicit t =>
        quantity * (price + tax)
      }

      assert(total.readValueOnce === 4)
      price.set(6)
      assert(total.readValueOnce === 8)
      quantity.set(2)
      assert(total.readValueOnce === 16)

    }

  }
}
