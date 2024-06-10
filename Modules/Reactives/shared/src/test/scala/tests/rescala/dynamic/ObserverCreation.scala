package tests.rescala.dynamic

import tests.rescala.testtools.RETests

class ObserverCreation extends RETests {
  import reactives.default.*
  {

    test("add Event After") {
      var res   = 0
      val e0    = Evt[Int]()(using "source")
      val e1    = e0.map(identity)(using "firstMap")
      val inner = e0.map { _ + 1 }(using "innerMap").observe { res = _ }(using "observer")
      e1.map(_ => inner)(using "creatingMap")
      e0.fire(10)

      assertEquals(res, 11)

    }

    test("event Handlers Can Be Removed") {
      var test = 0
      val e1   = Evt[Int]()(using "e1")
      val f    = (_: Int) => { test += 1 }
      val o    = e1.observe(f)(using "e1Observer")
      e1.fire(10)
      e1.fire(10)
      assert(test == 2)
      o.disconnect()
      e1.fire(10)
      assert(test == 2)
    }

  }
}
