package tests.rescala.events

import tests.rescala.RETests


class PredicateEventTest extends RETests {



  allEngines("predicate Event Is Executed Only If The Predicate Is True"){ engine => import engine._
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


}
