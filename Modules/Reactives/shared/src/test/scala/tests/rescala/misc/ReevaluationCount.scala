package tests.rescala.misc

class ReevaluationCount extends munit.FunSuite {

  import reactives.default.*
  {

    test("keep fixed Dependencies") {

      val v1   = Var(true)
      val v2   = Var(0)
      val v3   = Var(10)
      var i    = 0
      var test = 0

      val s = Signal {
        i += 1
        if v1.value then v2.value
        else v3.value
      }

      s.changed `observe` (_ => test += 1)

      assertEquals(test, 0)
      v2.set(1)
      assertEquals(test, 1)

      v1.set(false)
      assertEquals(test, 2)
      v3.set(11)
      assertEquals(test, 3)

      v2.set(2)
      assertEquals(test, 3)

      v1.set(true)
      assertEquals(test, 4)
      v2.set(3)
      assertEquals(test, 5)

    }

  }
}
