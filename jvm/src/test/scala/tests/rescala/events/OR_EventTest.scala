package tests.rescala.events


import tests.rescala.RETests




class OR_EventTest extends RETests {



  allEngines("handler Of_O R_IsExecuted IfAny OfThe Events Fires"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_OR_e2 = e1 || e2
    e1_OR_e2 += { _ => test += 1 }

    e1(10)
    e2(10)
    assert(test == 2)

  }

  allEngines("handler Of_O R_IsExecuted Only Once"){ engine => import engine._

    var test = 0
    val e1 = Evt[Int]
    val e2 = e1 map (_ * 2)
    val e3 = e1 map (_ * 2)
    val e2_OR_e3 = e2 || e3
    e1 += { _ => test += 1 }
    e2 += { _ => test += 1 }
    e3 += { _ => test += 1 }
    e2_OR_e3 += { _ => test += 1 }

    e1(10)
    assert(test == 4)
  }

}
