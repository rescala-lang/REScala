package tests.rescala.static.signals

import rescala.core.infiltration.Infiltrator
import tests.rescala.testtools.RETests
import rescala.interface.RescalaInterface
import rescala.scheduler.Levelbased

class SignalTestSuiteDotty extends RETests {
  multiEngined { engine =>
    val ie = new Infiltrator(engine.asInstanceOf[RescalaInterface with Levelbased])
    import ie.api._

    test("handler Is Called When Change Occurs") {

      var test = 0
      val v1   = Var(1)
      val v2   = Var(2)

      val s1 = Signals.lift(v1, v2) { _ + _ }
      s1.changed += { (_) => test += 1 }

      assert(s1.readValueOnce == 3)
      assert(test == 0)

      v2.set(3)
      assert(s1.readValueOnce == 4)
      assert(test == 1)

      v2.set(3)
      assert(s1.readValueOnce == 4)
      assert(test == 1)

    }

    test("signal Reevaluates The Expression When Something It Depends On Is Updated") {
      val v = Var(0)
      var i = 1
      val s = Signal { v.value + i }
      i = 2
      assert(s.readValueOnce == 1)
      v.set(2)
      assert(s.readValueOnce == 4)
    }
  }
}
