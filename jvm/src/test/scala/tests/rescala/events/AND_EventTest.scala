package tests.rescala.events

import tests.rescala.RETests



class AND_EventTest extends RETests {




  allEngines("handler Of_AN D_IsNOT Executed IfEvents Fire Singularly"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_AND_e2 = e1 zip e2
    e1_AND_e2 += ((x: (Int, Int)) => { test += 1 })

    e1(10)
    e2(10)
    assert(test == 0)


  }

  allEngines("handler Of_AN D_Does Not Remember Old Rounds"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_AND_e2 = e1 zip e2
    e1_AND_e2 += ((x: (Int, Int)) => { test += 1 })

    e1(10)
    e2(10)
    e1(10)
    e2(10)
    assert(test == 0)

  }

  allEngines("handler Of_AN D_IsExecuted IfBoth Events Fire"){ engine => import engine._

    var test = 0
    val e1 = Evt[Int]
    val e2 = e1 map ((x: Int) => x * 2)
    val e3 = e1 map ((x: Int) => x * 2)
    val e2_AND_e3 = e2 zip e3
    e1 += ((x: Int) => { test += 1 })
    e2 += ((x: Int) => { test += 1 })
    e3 += ((x: Int) => { test += 1 })
    e2_AND_e3 += ((x: (Int, Int)) => { test += 1 })

    e1(10)
    assert(test == 4)
  }

}
