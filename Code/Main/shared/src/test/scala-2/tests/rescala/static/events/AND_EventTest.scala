package tests.rescala.static.events

import tests.rescala.testtools.RETests

class AND_EventTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("handler Of AND Is NOT Executed If Events Fire Singularly") {
      var test      = 0
      val e1        = Evt[Int]()
      val e2        = Evt[Int]()
      val e1_AND_e2 = e1 zip e2
      e1_AND_e2 += ((_: (Int, Int)) => { test += 1 })

      assert(test == 0)
      e1.fire(10)
      assert(test == 0)
      e2.fire(10)
      assert(test == 0)

    }

    test("handler Of AND Does Not Remember Old Rounds") {
      var test      = 0
      val e1        = Evt[Int]()
      val e2        = Evt[Int]()
      val e1_AND_e2 = e1 zip e2
      e1_AND_e2 += ((_: (Int, Int)) => { test += 1 })

      e1.fire(10)
      e2.fire(10)
      e1.fire(10)
      e2.fire(10)
      assert(test == 0)

    }

    test("handler Of AND IsExecuted If Both Events Fire") {

      var test      = 0
      val e1        = Evt[Int]()
      val e2        = e1 map ((x: Int) => x * 2)
      val e3        = e1 map ((x: Int) => x * 2)
      val e2_AND_e3 = e2 zip e3
      e1 += ((_: Int) => { test += 1 })
      e2 += ((_: Int) => { test += 1 })
      e3 += ((_: Int) => { test += 1 })
      e2_AND_e3 += ((_: (Int, Int)) => { test += 1 })

      e1.fire(10)
      assert(test == 4)
    }

  }
}
