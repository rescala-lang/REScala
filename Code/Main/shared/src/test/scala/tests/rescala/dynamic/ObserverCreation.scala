package tests.rescala.dynamic

import tests.rescala.testtools.RETests

class ObserverCreation extends RETests {
  multiEngined { engine =>
    import engine._

    test("add Event After") {
      var res   = 0
      val e0    = Evt[Int]()("source")
      val e1    = e0.map(identity)("firstMap")
      val inner = e0.map { _ + 1 }("innerMap").observe { res = _ }("observer")
      e1.map(_ => inner)("creatingMap")
      e0.fire(10)

      assert(res === 11)

    }

    test("event Handlers Can Be Removed") {
      var test = 0
      val e1   = Evt[Int]()("e1")
      val f    = (_: Int) => { test += 1 }
      val o    = e1.observe(f)("e1Observer")
      e1.fire(10)
      e1.fire(10)
      assert(test == 2)
      o.disconnect()
      e1.fire(10)
      assert(test == 2)
    }

  }
}
