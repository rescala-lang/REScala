package tests.rescala.dynamic

import tests.rescala.testtools.RETests

class TrueDynamicEvents extends RETests {

  allEngines("higher order events") { engine => import engine._
    val e1 = Evt[Signal[Int]]

    val event =  Event.dynamic { Some{ e1.value.get.value } }

    val res = event.latest()

    e1.fire(Signal(1))
    assert(res.now === 1)
    e1.fire(Signal(2))
    assert(res.now === 2)
    e1.fire(Signal(3))
    assert(res.now === 3)

  }
}
