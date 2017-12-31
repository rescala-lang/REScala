package tests.rescala.events

import tests.rescala.RETests

class EventCreationTest extends RETests {





  allEngines("add Event After"){ engine => import engine._
    var res = 0
    val e0 = Evt[Int]
    val e1 = e0.map(identity)
    e1.map(_ => e0.map {_ + 1}.observe {res = _})
    e0.fire(10)

    assert(res === 11)

  }

}
