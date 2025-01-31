package tests.rescala.dynamic

import munit.FunSuite

class TrueDynamicEvents extends FunSuite {

  import reactives.default.*
  {

    test("higher order events") {
      val e1 = Evt[Signal[Int]]()

      val event = Event.dynamic { Some { e1.value.get.value } }

      val res = event.hold()

      e1.fire(Signal(1))
      assertEquals(res.readValueOnce, 1)
      e1.fire(Signal(2))
      assertEquals(res.readValueOnce, 2)
      e1.fire(Signal(3))
      assertEquals(res.readValueOnce, 3)

    }
  }
}
