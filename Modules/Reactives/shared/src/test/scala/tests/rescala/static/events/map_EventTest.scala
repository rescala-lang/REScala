package tests.rescala.static.events

import tests.rescala.testtools.RETests

class map_EventTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("handler Of map  Is Executed") {
      var test   = 0
      val e1     = Evt[Int]()
      val e1_map = e1 map ((x: Int) => x * 2)
      e1_map += ((_: Int) => { test += 1 })

      e1.fire(10)
      e1.fire(10)
      assert(test == 2)
    }

    test("the Function Passed To map is Applied") {
      var test   = 0
      val e1     = Evt[Int]()
      val e1_map = e1 map ((x: Int) => x * 2)
      e1_map += ((x: Int) => { test = x })

      e1.fire(10)
      e1.fire(10)
      assert(test == 20)
    }

  }
}
