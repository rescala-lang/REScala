package tests.rescala.static.events

import tests.rescala.testtools.RETests

class dropParam_EventTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("handler Of drop Param Is Executed") {
      var test                 = 0
      val e1                   = Evt[Int]()
      val e1_drop: Event[Unit] = e1.dropParam
      e1_drop += (_ => { test += 1; })

      e1.fire(10)
      e1.fire(10)
      assert(test == 2)
    }

  }
}
