package tests.rescala.static.events

import reactives.default.*
import tests.rescala.testtools.RETests

class EventTest extends RETests {

  test("handlers Are Executed") {
    var test = 0
    val e1   = Evt[Int]()
    e1 observe ((_: Int) => { test += 1 })
    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }

  test("correct Value Is Received") {
    var test = 0
    val e1   = Evt[Int]()
    e1 observe ((x: Int) => { test += x })
    e1.fire(10)
    assert(test == 10)
  }

  test("events Without Params Is Called") {
    var test = 0
    val e1   = Evt[Unit]()
    e1 observe (_ => { test += 1 })
    e1.fire(())
    assert(test == 1)
  }

  test("function Is Called") {
    var test = 0

    def f(): Unit = { test += 1 }

    val e1 = Evt[Int]()
    e1 observe (_ => f())

    e1.fire(10)
    e1.fire(10)
    assert(test == 2)
  }

  test("events With Method Handlers With Parameter") {

    var test       = 0
    val e          = Evt[Int]()
    def m1(): Unit = { test += 1 }

    e observe (_ => m1())
    e.fire(10)
    e.fire(10)
    assert(test == 2)

  }

  test("from callback in transaction test") {

    val res = transaction() { implicit at =>
      val res = Event.fromCallback {
        Event.handle("some!")
      }
      res.event.hold("none")
    }

    assertEquals(res.readValueOnce, "some!")

  }

}
