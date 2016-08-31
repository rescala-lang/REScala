package tests.rescala.events

class except_EventTest extends tests.rescala.RETests {



  allEngines("handler Of_except_ IsExecuted IfBasic Event Fires"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ((x: Int) => { test += 1 })

    e1(10)
    assert(test == 1)

  }

  allEngines("handler Of_except_ Ignores The Second Event IfFires"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ((x: Int) => { test += 1 })

    e2(10)
    assert(test == 0)

  }

  allEngines("handler Of_except_ IsExecuted Only IfFirst Event Fires And Not The Second"){ engine => import engine._

    var test = 0

    var cond = false
    val e1 = Evt[Int]
    val e2 = e1 map ((x: Int) => x * 2)
    val e3 = e1 filter (_ => cond)
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ((x: Int) => { test += 1 })


    e1(10)
    assert(test == 1)

    cond = true
    e1(10)
    assert(test == 1)

    cond = false
    e1(10)
    assert(test == 2)

  }


  allEngines("handler Of_except_ Gets The Correct Value"){ engine => import engine._

    var value = 0

    var cond = false
    val e1 = Evt[Int]
    val e2 = e1 map ((x: Int) => x)
    val e3 = (e1 map ((x: Int) => x * 2)) filter (_ => cond)
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ((x: Int) => { value = x })


    e1(10)
    assert(value == 10)

    cond = true
    e1(11)
    assert(value == 10)

    cond = false
    e1(12)
    assert(value == 12)

  }

}
