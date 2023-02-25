package tests.rescala.static.events

import java.util.concurrent.atomic.AtomicInteger
import rescala.structure.RExceptions.ObservedException
import rescala.structure.Pulse
import tests.rescala.testtools.RETests

class OR_EventTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("handler Of OR Is Executed If Any Of The Events Fires") {
      var test     = 0
      val e1       = Evt[Int]()
      val e2       = Evt[Int]()
      val e1_OR_e2 = e1 || e2
      e1_OR_e2 observe { _ => test += 1 }

      assert(test == 0)
      e1.fire(10)
      assert(test == 1)
      e2.fire(10)
      assert(test == 2)

    }

    test("handler Of OR Is Executed Only Once") {

      val test     = new AtomicInteger(0)
      val e1       = Evt[Int]()
      val e2       = e1 map (_ * 2)
      val e3       = e1 map (_ * 2)
      val e2_OR_e3 = e2 || e3
      e1 observe { _ =>
        test.incrementAndGet(); ()
      }
      e2 observe { _ =>
        test.incrementAndGet(); ()
      }
      e3 observe { _ =>
        test.incrementAndGet(); ()
      }
      e2_OR_e3 observe { _ =>
        test.incrementAndGet(); ()
      }

      e1.fire(10)
      assert(test.get == 4)
    }

    test("OR event select correct event") {
      val e1 = Evt[String]()
      val e2 = Evt[String]()
      val e3 = e1 || e2

      val log = e3.list()

      assert(log.readValueOnce === Nil)

      e1.fire("one")
      assert(log.readValueOnce === List("one"))

      e2.fire("two")
      assert(log.readValueOnce === List("two", "one"))

      transaction(e1, e2) { implicit turn =>
        e1.admit("three a")
        e2.admit("three b")
      }

      assert(log.readValueOnce === List("three a", "two", "one"))

      transaction(e1, e2) { turn =>
        e1.admitPulse(Pulse.Exceptional(new IllegalArgumentException))(turn)
        e2.admit("five b")(turn)
      }

      intercept[ObservedException](log.readValueOnce)

    }

  }
}
