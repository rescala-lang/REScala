package tests.rescala.events

import tests.rescala.RETests


class map_EventTest extends RETests {



  allEngines("handler Of map  Is Executed"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e1_map = e1 map ((x: Int) => x * 2)
    e1_map += ((x: Int) => { test += 1 })

    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }

  allEngines("the Function Passed To map is Applied"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e1_map = e1 map ((x: Int) => x * 2)
    e1_map += ((x: Int) => { test = x })

    e1.fire(10)
    e1.fire(10)
    assert(test == 20)
  }

}
