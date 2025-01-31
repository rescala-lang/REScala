package tests.rescala.static.events

class map_EventTest extends munit.FunSuite {

  import reactives.default.*
  {

    test("handler Of map  Is Executed") {
      var test   = 0
      val e1     = Evt[Int]()
      val e1_map = e1 `map` ((x: Int) => x * 2)
      e1_map `observe` ((_: Int) => { test += 1 })

      e1.fire(10)
      e1.fire(10)
      assertEquals(test, 2)
    }

    test("the Function Passed To map is Applied") {
      var test   = 0
      val e1     = Evt[Int]()
      val e1_map = e1 `map` ((x: Int) => x * 2)
      e1_map `observe` ((x: Int) => { test = x })

      e1.fire(10)
      e1.fire(10)
      assertEquals(test, 20)
    }

  }
}
