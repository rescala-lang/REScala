package tests.rescala.static.events

import tests.rescala.testtools.RETests


class EventTest extends RETests { multiEngined { engine => import engine._



  test("handlers Are Executed"){
    var test = 0
    val e1 = Evt[Int]
    e1 += ((x: Int) => { test += 1 })
    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }

  test("correct Value Is Received"){
    var test = 0
    val e1 = Evt[Int]
    e1 += ((x: Int) => { test += x })
    e1.fire(10)
    assert(test == 10)
  }

  test("events Without Params Is Called"){
    var test = 0
    val e1 = Evt[Unit]
    e1 += (_ => { test += 1 })
    e1.fire(())
    assert(test == 1)
  }


  test("function Is Called"){
    var test = 0

    def f(x: Int): Unit = { test += 1 }

    val e1 = Evt[Int]
    e1 += f

    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }


  test("events With Method Handlers With Parameter"){

    var test = 0
    val e = Evt[Int]
    def m1(x: Int): Unit = { test += 1 }

    e += m1
    e.fire(10)
    e.fire(10)
    assert(test == 2)

  }

} }
