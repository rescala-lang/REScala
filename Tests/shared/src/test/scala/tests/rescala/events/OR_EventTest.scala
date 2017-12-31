package tests.rescala.events

import java.util.concurrent.atomic.AtomicInteger

import rescala.core.Pulse
import tests.rescala.RETests


class OR_EventTest extends RETests {


  allEngines("handler Of OR Is Executed If Any Of The Events Fires") { engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_OR_e2 = e1 || e2
    e1_OR_e2 += { _ => test += 1 }

    e1.fire(10)
    e2.fire(10)
    assert(test == 2)

  }

  allEngines("handler Of OR Is Executed Only Once") { engine => import engine._

    val test = new AtomicInteger(0)
    val e1 = Evt[Int]
    val e2 = e1 map (_ * 2)
    val e3 = e1 map (_ * 2)
    val e2_OR_e3 = e2 || e3
    e1 += { _ => test.incrementAndGet() }
    e2 += { _ => test.incrementAndGet() }
    e3 += { _ => test.incrementAndGet() }
    e2_OR_e3 += { _ => test.incrementAndGet() }

    e1.fire(10)
    assert(test.get == 4)
  }

  allEngines("OR event select correct event") { engine => import engine._
    val e1 = Evt[String]
    val e2 = Evt[String]

    val e3 = e1 || e2

    val log = e3.list()

    assert(log.now === Nil)

    e1.fire("one")
    assert(log.now === List("one"))

    e2.fire("two")
    assert(log.now === List("two", "one"))


    update(e1 -> "three a", e2 -> "three b")

    assert(log.now === List("three a", "two", "one"))


    transaction(e1, e2) { turn =>
      e1.admitPulse(Pulse.Exceptional(new IllegalArgumentException))(turn)
      e2.admit("five b")(turn)
    }

    intercept[IllegalStateException](log.now)

  }

}
