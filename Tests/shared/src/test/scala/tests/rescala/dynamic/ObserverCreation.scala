package tests.rescala.dynamic

import tests.rescala.testtools.RETests


class ObserverCreation extends RETests {

  allEngines("add Event After"){ engine => import engine._
    var res = 0
    val e0 = Evt[Int]
    val e1 = e0.map(identity)
    e1.map(_ => e0.map {_ + 1}.observe {res = _})
    e0.fire(10)

    assert(res === 11)

  }

  allEngines("event Handlers Can Be Removed"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val f = (x: Int) => { test += 1 }
    val o = e1 += f
    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
    o.remove()
    e1.fire(10)
    assert(test == 2)
  }

}
