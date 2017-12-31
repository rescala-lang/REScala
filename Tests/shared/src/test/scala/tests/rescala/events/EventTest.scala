package tests.rescala.events

import tests.rescala.RETests



class EventTest extends RETests {



  allEngines("handlers Are Executed"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    e1 += ((x: Int) => { test += 1 })
    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
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

  allEngines("correct Value Is Received"){ engine => import engine._
    var test = 0
    val e1 = Evt[Int]
    e1 += ((x: Int) => { test += x })
    e1.fire(10)
    assert(test == 10)
  }

  allEngines("events Without Params Is Called"){ engine => import engine._
    var test = 0
    val e1 = Evt[Unit]
    e1 += (_ => { test += 1 })
    e1.fire(())
    assert(test == 1)
  }


  allEngines("function Is Called"){ engine => import engine._
    var test = 0

    def f(x: Int): Unit = { test += 1 }

    val e1 = Evt[Int]
    e1 += f

    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }


  allEngines("events With Method Handlers With Parameter"){ engine => import engine._

    var test = 0
    val e = Evt[Int]
    def m1(x: Int): Unit = { test += 1 }

    e += m1
    e.fire(10)
    e.fire(10)
    assert(test == 2)

  }

}
