package tests.rescala.static.events

import tests.rescala.testtools.RETests

class PredicateEventTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("predicate Event Is Executed Only If The Predicate Is True") {
      var test = 0
      var cond = false
      val e1   = Evt[Int]()
      val e2   = e1 filter ((_: Int) => cond)
      e2 observe ((_: Int) => { test += 1 })

      e1.fire(10)
      e1.fire(10)
      e1.fire(10)
      assert(test == 0)
      cond = true
      e1.fire(10)
      e1.fire(10)
      assert(test == 2)
    }

    test("collect filters values") {
      var test = 0
      var cond = false
      val e1   = Evt[Int]()
      val e2 = e1.collect {
        case e if cond => e
      }
      e2 observe ((_: Int) => { test += 1 })

      e1.fire(10)
      e1.fire(10)
      e1.fire(10)
      assert(test == 0)
      cond = true
      e1.fire(10)
      e1.fire(10)
      assert(test == 2)
    }

    test("collect maps and filters values") {
      val e1 = Evt[String]()
      val e2 = e1.collect {
        case "accept" => true
      }

      val count  = e2.count()
      val result = e2.hold(false)
      assertEquals(count.readValueOnce, 0)
      assert(!result.readValueOnce)
      e1.fire("reject")
      assertEquals(count.readValueOnce, 0)
      assert(!result.readValueOnce)
      e1.fire("accept")
      assertEquals(count.readValueOnce, 1)
      assert(result.readValueOnce)

    }

  }
}
