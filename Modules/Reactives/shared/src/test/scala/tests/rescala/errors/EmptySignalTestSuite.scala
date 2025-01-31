package tests.rescala.errors

import munit.FunSuite

class EmptySignalTestSuite extends FunSuite {

  import reactives.default.*
  {

    test("basic Empty Signal Test") {

      val v = Var.empty[Int]

      intercept[NoSuchElementException](v.readValueOnce)

      var res = -100

      v.observe(res = _)

      assertEquals(res, -100, "sanity")

      val s = v.map(1.+)

      intercept[NoSuchElementException](s.readValueOnce)

      v.set(100)

      assertEquals(res, 100, "observed?")
      assertEquals(v.readValueOnce, 100, "changed from empty to value")
      assertEquals(s.readValueOnce, 101, "changed from empty to value 2")

    }

    test("flatten empty signal when mapping event") {

      val v = Var.empty[Event[Unit]]

      val e1 = Evt[Unit]()

      val flat = v.flatten
      val e2   = e1 map { _ => flat.count() }

      var s: Signal[Int] = null

      e2.observe(s = _)

      e1.fire()

      assert(s != null, "sanity")

      assertEquals(s.readValueOnce, 0, "mapped event")

      val e3 = Evt[Unit]()
      v.set(e3)

      assertEquals(s.readValueOnce, 0, "mapped event after var set")

      e3.fire()

      assertEquals(s.readValueOnce, 1, "mapped event after event fire")
    }

    test("unwrap Empty Signal") {
      val v = Var.empty[Event[Int]]
      val e = v.flatten

      var res = -100

      e.observe(res = _)

      assertEquals(res, -100, "sanity")

      val evt = Evt[Int]()
      v.set(evt)

      evt.fire(20)

      assertEquals(res, 20, "could unwrap after Val was empty")

    }

    test("propagate emptiness") {
      val v      = Var[Int](6)
      val v2     = Var[Int](6)
      val sig    = Signal { v.value + v2.value }
      val e      = sig.changed
      val folded = e.fold(0)(_ - _)

      assertEquals(v.readValueOnce, 6)
      assertEquals(sig.readValueOnce, v.readValueOnce + v2.readValueOnce)
      assertEquals(folded.readValueOnce, 0)

      v.setEmpty()
      intercept[NoSuchElementException](v.readValueOnce)
      intercept[NoSuchElementException](sig.readValueOnce)
      assertEquals(v2.readValueOnce, 6)
      assertEquals(folded.readValueOnce, 0)

      v.set(10)
      assertEquals(v.readValueOnce, 10)
      assertEquals(v2.readValueOnce, 6)
      assertEquals(sig.readValueOnce, v.readValueOnce + v2.readValueOnce)
      assertEquals(folded.readValueOnce, -16)

    }

  }
}
