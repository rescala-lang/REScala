package tests.rescala.errors

import tests.rescala.testtools.RETests

class EmptySignalTestSuite extends RETests {
  multiEngined { engine =>
    import engine._

    test("basic Empty Signal Test") {

      val v = Var.empty[Int]

      intercept[NoSuchElementException](v.readValueOnce)

      var res = -100

      v.observe(res = _)

      assert(res == -100, "sanity")

      val s = v.map(1.+)

      intercept[NoSuchElementException](s.readValueOnce)

      v.set(100)

      assert(res == 100, "observed?")
      assert(v.readValueOnce == 100, "changed from empty to value")
      assert(s.readValueOnce == 101, "changed from empty to value 2")

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

      assert(s.readValueOnce == 0, "mapped event")

      val e3 = Evt[Unit]()
      v.set(e3)

      assert(s.readValueOnce == 0, "mapped event after var set")

      e3.fire()

      assert(s.readValueOnce == 1, "mapped event after event fire")
    }

    test("unwrap Empty Signal") {
      val v = Var.empty[Event[Int]]
      val e = v.flatten

      var res = -100

      e.observe(res = _)

      assert(res === -100, "sanity")

      val evt = Evt[Int]()
      v.set(evt)

      evt.fire(20)

      assert(res === 20, "could unwrap after Val was empty")

    }

    test("propagate emptiness") {
      val v      = Var[Int](6)
      val v2     = Var[Int](6)
      val sig    = Signal { v() + v2() }
      val e      = sig.changed
      val folded = e.fold(0)(_ - _)

      assert(v.readValueOnce === 6)
      assert(sig.readValueOnce === v.readValueOnce + v2.readValueOnce)
      assert(folded.readValueOnce === 0)

      v.setEmpty()
      intercept[NoSuchElementException](v.readValueOnce)
      intercept[NoSuchElementException](sig.readValueOnce)
      assert(v2.readValueOnce === 6)
      assert(folded.readValueOnce === 0)

      v.set(10)
      assert(v.readValueOnce === 10)
      assert(v2.readValueOnce === 6)
      assert(sig.readValueOnce === v.readValueOnce + v2.readValueOnce)
      assert(folded.readValueOnce === -16)

    }

  }
}
