package tests.rescala.static.signals

import reactives.SelectedScheduler
import reactives.core.infiltration.Infiltrator
import reactives.scheduler.Levelbased
import tests.rescala.testtools.RETests

import java.util.concurrent.atomic.AtomicInteger

class SignalTestSuite extends RETests {
  multiEngined { engine =>
    val ie = new Infiltrator()
    import ie.api.*
    import ie.assertLevel

    test("handler Is Called When Change Occurs") {

      var test = 0
      val v1   = Var(1)
      val v2   = Var(2)

      val s1 = Signal.lift(v1, v2) { _ + _ }
      s1.changed observe { (_) => test += 1 }

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

    test("the Expression Is Not Evaluated Every Time now Is Called") {
      var a = 10
      val s = Signal(1 + 1 + a)
      assertEquals(s.readValueOnce, 12)
      a = 11
      assertEquals(s.readValueOnce, 12)
    }

    test("level Is Correctly Computed") {

      val v = Var(1)

      val s1 = Signal { 2 * v.value }
      val s2 = Signal { 3 * v.value }
      val s3 = Signal { s1.value + s2.value }

      assertLevel(v, 0)
      assertLevel(s1, 1)
      assertLevel(s2, 1)
      assertLevel(s3, 2)

    }

    test("dependant Is Only Invoked On Value Changes") {
      var changes = 0
      val v       = Var(1)
      val s = Signal {
        changes += 1; v.value + 1
      }
      assertEquals(changes, 1)
      assertEquals(s.readValueOnce, 2)
      v.set(2)
      assertEquals(s.readValueOnce, 3)
      assertEquals(changes, 2)
      v.set(2)
      assertEquals(changes, 2) // is actually 3
    }

    test("creating signals in signals based on changing signals") {
      val v0 = Var("level 0")
      val v3 = v0.map(_ + "level 1").map(_ + "level 2").map(_ + "level 3")

      val `dynamic signal changing from level 1 to level 5` = Signal {
        if v0.value == "level 0" then v0.value
        else {
          v3.map(_ + "level 4 inner").value
        }
      }
      assert(`dynamic signal changing from level 1 to level 5`.readValueOnce == "level 0")
      // note: will start with level 5 because of static guess of current level done by the macro expansion
      assertLevel(`dynamic signal changing from level 1 to level 5`, 5)

      v0.set("level0+")
      assert(
        `dynamic signal changing from level 1 to level 5`.readValueOnce == "level0+level 1level 2level 3level 4 inner"
      )
      assertLevel(`dynamic signal changing from level 1 to level 5`, 5)
    }

    test("signal Reevaluates The Expression") {
      val v              = Var(0)
      var i              = 1
      val s: Signal[Int] = v.map { _ => i }
      i = 2
      v.set(2)
      assert(s.readValueOnce == 2)
    }

    test("the Expression Is Note Evaluated Every Time Get Val Is Called") {
      var a              = 10
      val s: Signal[Int] = Signal.static()(_ => 1 + 1 + a)
      assertEquals(s.readValueOnce, 12)
      a = 11
      assertEquals(s.readValueOnce, 12)
    }

    test("simple Signal Returns Correct Expressions") {
      val s: Signal[Int] = Signal.static()(_ => 1 + 1 + 1)
      assertEquals(s.readValueOnce, 3)
    }

    test("the Expression Is Evaluated Only Once") {

      var a = 0
      val v = Var(10)
      val s1: Signal[Int] = v.map { i =>
        a += 1
        i % 10
      }

      assert(a == 1)
      v.set(11)
      assert(a == 2)
      v.set(21)
      assert(a == 3)
      assertEquals(s1.readValueOnce, 1)
    }

    test("handlers Are Executed") {

      val test = new AtomicInteger(0)
      val v    = Var(1)

      val s1 = v.map { 2 * _ }
      val s2 = v.map { 3 * _ }
      val s3 = Signal.lift(s1, s2) { _ + _ }

      s1.changed observe { (_) =>
        test.incrementAndGet(); ()
      }
      s2.changed observe { (_) =>
        test.incrementAndGet(); ()
      }
      s3.changed observe { (_) =>
        test.incrementAndGet(); ()
      }

      assert(test.get == 0)

      v.set(3)
      assert(test.get == 3)
    }

    test("level Is Correctly Computed with combinators") {

      val v = Var(1)

      val s1 = v.map { 2 * _ }
      val s2 = v.map { 3 * _ }
      val s3 = Signal.lift(s1, s2) { _ + _ }

      assertLevel(v, 0)
      assertLevel(s1, 1)
      assertLevel(s2, 1)
      assertLevel(s3, 2)
    }

    test("no Change Propagation") {
      val v  = Var(1)
      val s  = v.map(_ => 1)
      val s2 = Signal { s.value }

      assertEquals(s2.readValueOnce, 1)
      assertEquals(s.readValueOnce, 1)

      v.set(2)
      assertEquals(s.readValueOnce, 1)
      assertEquals(s2.readValueOnce, 1)

      v.set(2)
      assertEquals(s2.readValueOnce, 1)
      assertEquals(s.readValueOnce, 1)

      v.set(3)
      assertEquals(s2.readValueOnce, 1)
      assertEquals(s.readValueOnce, 1)

    }
  }
}
