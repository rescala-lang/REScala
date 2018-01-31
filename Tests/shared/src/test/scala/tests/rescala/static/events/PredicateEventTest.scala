package tests.rescala.static.events

import tests.rescala.testtools.RETests


class PredicateEventTest extends RETests { multiEngined { engine => import engine._



  test("predicate Event Is Executed Only If The Predicate Is True"){
    var test = 0
    var cond = false
    val e1 = Evt[Int]
    val e2 = e1 filter ((x: Int) => cond)
    e2 += ((x: Int) => { test += 1 })

    e1.fire(10)
    e1.fire(10)
    e1.fire(10)
    assert(test == 0)
    cond = true
    e1.fire(10)
    //e1.fire(10)
    //assert(test == 2)
  }


} }
