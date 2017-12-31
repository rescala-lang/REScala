package tests.rescala.events

import tests.rescala.RETests




class dropParam_EventTest extends RETests {



  allEngines("handler Of drop Param  Is Executed"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e1_drop: Event[Unit] = e1.dropParam
    e1_drop += ((x) => { test += 1; })

    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }

}
