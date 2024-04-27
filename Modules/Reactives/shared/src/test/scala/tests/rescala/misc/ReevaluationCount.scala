package tests.rescala.misc

import tests.rescala.testtools.RETests

class ReevaluationCount extends RETests {
  multiEngined { engine =>
    import engine.*

    test("keep fixed Dependencies") {

      val v1   = Var(true)
      val v2   = Var(0)
      val v3   = Var(10)
      var i    = 0
      var test = 0

      val s = Signal {
        i += 1
        if (v1.value) v2.value else v3.value
      }

      s.changed observe (_ => test += 1)

      assert(test == 0)
      v2.set(1)
      assert(test == 1)

      v1.set(false)
      assert(test == 2)
      v3.set(11)
      assert(test == 3)

      v2.set(2)
      assert(test == 3)

      v1.set(true)
      assert(test == 4)
      v2.set(3)
      assert(test == 5)

    }

  }
}
