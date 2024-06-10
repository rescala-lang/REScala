package tests.rescala.static.signals

import tests.rescala.testtools.RETests

//These 3 are for JUnitRunner

class VarTestSuite extends RETests {
import reactives.default.*
{

    test("get Val After Creation Returns Initialization Value") {
      val v = Var(1)
      assert(v.readValueOnce == 1)
    }

    test("get Val Returns Correct Value") {
      val v = Var(1)
      v.set(10)
      assert(v.readValueOnce == 10)
    }

    test("var Notifies Signal Of Changes") {
      val v = Var(1)
      val s = v.map { _ + 1 }
      assert(v.readValueOnce == 1)

      assert(s.readValueOnce == 2)
      v.set(2)
      assert(v.readValueOnce == 2)
      assert(s.readValueOnce == 3)

    }

    test("change Event Only Triggered On Value Change") {
      var changes = 0
      val v       = Var(1)
      v.observe { _ => changes += 1 }

      assert(changes == 1)
      v.set(2)
      assert(changes == 2)
      v.set(3)
      assert(changes == 3)
      v.set(3)
      assert(changes == 3)
    }

    test("dependant Is Only Invoked On Value Change") {
      var changes = 0
      val v       = Var(1)
      val s = v.map { i =>
        changes += 1; i + 1
      }
      assert(s.readValueOnce == 2)
      assert(changes == 1)
      v.set(2)
      assert(s.readValueOnce == 3)
      assert(changes == 2)
      v.set(2)
      assert(changes == 2)
    }

    test("transform Var") {
      val v1    = Var(0)
      def inc() = v1.transform(1.+)

      assertEquals(v1.readValueOnce, 0)
      inc()
      assertEquals(v1.readValueOnce, 1)

      val s1 = v1.map(identity)

      assertEquals(s1.readValueOnce, 1)
      inc()
      assertEquals(v1.readValueOnce, 2)
      assertEquals(s1.readValueOnce, 2)

    }

  }
}
