package tests.rescala.static.events

import tests.rescala.testtools.RETests

class MacroEventTestSuite extends RETests {
  multiEngined { engine =>
    import engine._

    test("simple") {
      val ev1 = Evt[Int]()
      val v1  = Var(8)
      val snapshotEvent = Event {
        ev1.value.map(i => i + v1.value)
      }

      val res = snapshotEvent.hold(0)

      assert(res.readValueOnce === 0)
      ev1.fire(10)
      assert(res.readValueOnce === 18)
      v1.set(7)
      assert(res.readValueOnce === 18)
      ev1.fire(10)
      assert(res.readValueOnce === 17)

    }

    test("map") {
      val ev1           = Evt[Int]()
      val v1            = Var(8)
      val snapshotEvent = ev1.map(i => i + v1.value)

      val res = snapshotEvent.hold(0)

      assert(res.readValueOnce === 0)
      ev1.fire(10)
      assert(res.readValueOnce === 18)
      v1.set(7)
      assert(res.readValueOnce === 18)
      ev1.fire(10)
      assert(res.readValueOnce === 17)

    }

    test("map as static") {
      val ev1           = Evt[Int]()
      val snapshotEvent = ev1.map(i => i + 1)

      val res = snapshotEvent.hold(0)

      assert(res.readValueOnce === 0)
      ev1.fire(10)
      assert(res.readValueOnce === 11)
      ev1.fire(20)
      assert(res.readValueOnce === 21)
      ev1.fire(10)
      assert(res.readValueOnce === 11)

    }

    test("use Events In Signal Expression") {
      val e1  = Evt[Int]()
      val e2  = Evt[Int]()
      val res = Signal { List(e1.value, e2.value).flatten.sum }

      assert(res.readValueOnce === 0)
      e1.fire(10)
      assert(res.readValueOnce === 10)
      e2.fire(11)
      assert(res.readValueOnce === 11)
      transaction(e1, e2) { implicit at =>
        e1.admit(10)
        e2.admit(10)
      }
      assert(res.readValueOnce === 20)

    }

    test("use Event Expression") {
      val e1    = Evt[Int]()
      val e2    = Evt[Int]()
      val event = Event { Some(List(e1.value, e2.value).flatten) }
      val res   = event.hold(Nil)

      assert(res.readValueOnce === Nil)
      e1.fire(9)
      assert(res.readValueOnce === List(9))
      e2.fire(10)
      assert(res.readValueOnce === List(10))
      transaction(e1, e2) { implicit at =>
        e1.admit(11)
        e2.admit(12)
      }
      assert(res.readValueOnce === List(11, 12))
    }

    test("cut out created signals") {
      val e1    = Evt[Int]()
      val event = Event { Some { e1.count().value } }

      val res = event.hold()

      e1.fire(1)
      assert(res.readValueOnce === 1)
      e1.fire(2)
      assert(res.readValueOnce === 2)
      e1.fire(3)
      assert(res.readValueOnce === 3)

    }

  }
}
