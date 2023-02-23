package tests.rescala.dynamic

import tests.rescala.testtools.RETests

import scala.concurrent.Future

class FlattenTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("flatten var") {
      val sv = Signal { Var(10) }.flatten
      val vv = Var(Var(10)).flatten
      val vs = Var(Signal { 10 }).flatten
      val ss = Signal(Signal(10)).flatten
      assert(sv.readValueOnce === 10)
      assert(vv.readValueOnce === 10)
      assert(vs.readValueOnce === 10)
      assert(ss.readValueOnce === 10)
    }

    test("flatten array") {
      val sv = Signal { Array(Var(10)) }.flatten
      assert(sv.readValueOnce sameElements Array(10))

    }

    test("creating Signals Inside Signals and flattening") {

      val outside = Var(1)

      val dynsig: Signal[Signal[Int]] = Signal { Signal { outside() } }
      val testsig                     = dynsig.flatten

      assert(testsig.readValueOnce === 1)
      outside set 2
      assert(testsig.readValueOnce === 2)
    }

    test("flatten Signal Seq") {
      val v          = Var.empty[Seq[Signal[Int]]]
      var count      = 0
      val v1, v2, v3 = { count += 1; Var(count) }
      v.set(List(v1, v2, v3))

      val flat = v.flatten

      assert(flat.readValueOnce === Seq(1, 2, 3), "flatten fails")

      v2.set(100)

      assert(flat.readValueOnce === Seq(1, 100, 3), "flatten fails 2")

      v.set(List(v3, v2))

      assert(flat.readValueOnce === Seq(3, 100), "flatten fails 3")
    }

    test("flatten Signal Set") {
      val v          = Var.empty[Set[Var[Int]]]
      var count      = 0
      val v1, v2, v3 = { count += 1; Var(count) }
      v.set(Set(v1, v2, v3))

      val flat = v.flatten

      assert(flat.readValueOnce === Set(1, 2, 3), "flatten fails")

      v2.set(100)

      assert(flat.readValueOnce === Set(1, 100, 3), "flatten fails 2")

      v.set(Set(v3, v2))

      assert(flat.readValueOnce === Set(3, 100), "flatten fails 3")
    }

    test("flatten Signal Array") {
      val v          = Var.empty[Array[Var[Int]]]
      var count      = 0
      val v1, v2, v3 = { count += 1; Var(count) }
      v.set(Array(v1, v2, v3))

      val flat = v.flatten

      assert(flat.readValueOnce === Array(1, 2, 3), "flatten fails")

      v2.set(100)

      assert(flat.readValueOnce === Array(1, 100, 3), "flatten fails 2")

      v.set(Array(v3, v2))

      assert(flat.readValueOnce === Array(3, 100), "flatten fails 3")
    }

    test("flatten Signal Option") {
      val v = Var(Option.empty[Var[Int]])
      val w = Var(1)

      val flat: Signal[Option[Int]] = v.flatten

      assert(flat.readValueOnce === None, "flatten fails")

      v.set(Some(w))

      assert(flat.readValueOnce === Some(1), "flatten fails 2")

      w.set(100)

      assert(flat.readValueOnce === Some(100), "flatten fails 3")
    }

    test("flatten Event") {
      val e1            = Evt[Int]()
      val condition     = e1.hold(-1)
      val level1Event   = e1.map(_ => "level 1")
      val level2Event   = level1Event.map(_ => "level 2")
      val dynamicSignal = Signal { if (condition() == 1) level1Event else level2Event }

      val unwrapped = dynamicSignal.flatten

      var log = List[String]()
      unwrapped observe (log ::= _)

      e1.fire(0)
      assert(log == List("level 2"))
      e1.fire(1)
      assert(log == List("level 1", "level 2"))
    }

    test("flatten Event Same Level") {
      val e1              = Evt[Int]()
      val level2Condition = e1.hold(-1).map(identity)
      val level1EventA    = e1.map(_ => "A")
      val level1EventB    = e1.map(_ => "B")
      val dynamicSignal   = Signal { if (level2Condition() == 1) level1EventA else level1EventB }

      val unwrapped = dynamicSignal.flatten

      var log = List[String]()
      unwrapped observe (log ::= _)

      e1.fire(0)
      assert(log == List("B"))
      e1.fire(1)
      assert(log == List("A", "B"))
    }

    test("unwrap  Event") {
      val e1            = Evt[Int]()
      val e2            = Evt[Int]()
      val eventSelector = Var(e1)
      val selected      = eventSelector.map(identity)
      val unwrapped     = selected.flatten

      var lastEvent = -1
      unwrapped observe { lastEvent = _ }

      e1.fire(1)
      assert(lastEvent == 1)
      e2.fire(2)
      assert(lastEvent == 1)
      eventSelector set e2 // select new event source
      e2.fire(3)
      assert(lastEvent == 3)
      e1.fire(4)
      assert(lastEvent == 3)
      e2.fire(5)
      assert(lastEvent == 5)
    }

    test("dynamic Level") {
      val v1 = Var(1)

      val derived = v1.map(identity)

      val level1 = v1.map(_ + 1)
      val level2 = level1.map(_ + 1)
      val level3 = level2.map(_ + 1)

      val combined = Signal { if (v1() == 10) level3() else derived() }

      var log = List[Int]()

      combined.changed observe (log ::= _)

      v1.set(10)
      assert(log == List(13))
      v1.set(1)
      assert(log == List(1, 13))

      val higherOrder = Signal { if (v1() == 10) level3 else derived }
      val flattened   = higherOrder.flatten

      var higherOrderLog = List[Int]()

      flattened.changed observe (higherOrderLog ::= _)

      v1.set(10)
      assert(higherOrderLog == List(13))
      v1.set(1)
      assert(higherOrderLog == List(1, 13))
      assert(log == List(1, 13, 1, 13))
    }

    test("basic Higher Order Signal can be dereferenced") {
      val v                       = Var(42)
      val s1: Signal[Int]         = v.map(identity)
      val s2: Signal[Signal[Int]] = Signal.dynamic() { _ => s1 }
      val sDeref                  = s2.flatten

      assert(sDeref.readValueOnce == 42)

      v.set(0)
      assert(sDeref.readValueOnce == 0)
    }

    test("basic Higher Order Signal deref Fires Change") {
      val v                            = Var(42)
      val sValue: Signal[Int]          = v.map(identity)
      val sHigher: Signal[Signal[Int]] = Signal.dynamic() { _ => sValue }
      val sDeref                       = sHigher.flatten

      var sDerefChanged  = false
      var sHigherChanged = false

      sDeref.change observe { _ => sDerefChanged = true }
      sHigher.change observe { _ => sHigherChanged = true }

      assert(!sHigherChanged && !sDerefChanged)

      v.set(0)                // update
      assert(!sHigherChanged) // higher does not change
      assert(sDerefChanged)   // deref DOES change
    }

    test("basic Higher Order Signal higher Order Fires Change") {
      val v1              = Var(42)
      val v2              = Var(123)
      val s1: Signal[Int] = v1.map(identity)
      val s2: Signal[Int] = v2.map(identity)

      val selector: Var[Signal[Int]] = Var(s1)
      val sHigher                    = selector.map(identity)
      val sDeref                     = sHigher.flatten

      var sDerefChanged  = false
      var sHigherChanged = false

      sDeref.change observe { _ => sDerefChanged = true }
      sHigher.change observe { _ => sHigherChanged = true }

      // 1. Unrelated value changes, no updates
      v2.set(1234)
      assert(!sDerefChanged)
      assert(!sHigherChanged)

      // 2. Related value changes, only the deref signal changes
      v1.set(321)
      assert(sDerefChanged)
      assert(!sHigherChanged)
      sDerefChanged = false

      // 3. Selector changes, both signals fire changes
      selector set s2
      assert(sDerefChanged)
      assert(sHigherChanged)

      assert(sDeref.readValueOnce == 1234)
    }

    test("order3 Signal") {

      val v                               = Var(42)
      val s0: Signal[Int]                 = v.map(identity)
      val s1: Signal[Signal[Int]]         = Signal.static() { _ => s0 }
      val s2: Signal[Signal[Signal[Int]]] = Signal.static() { _ => s1 }

      val sDeref1   = s1.flatten
      val sDeref2   = s2.flatten.flatten
      val sDeref2_a = s2.flatten
      val sDeref2_b = sDeref2_a.flatten

      var sDeref1Changed   = false
      var sDeref2Changed   = false
      var sDeref2_aChanged = false
      var sDeref2_bChanged = false

      sDeref1.change observe { _ => sDeref1Changed = true }
      sDeref2.change observe { _ => sDeref2Changed = true }
      sDeref2_a.change observe { _ => sDeref2_aChanged = true }
      sDeref2_b.change observe { _ => sDeref2_bChanged = true }

      v.set(0)
      assert(sDeref1Changed)
      assert(sDeref2Changed)
      assert(!sDeref2_aChanged) // 2_a is not completely dereferenced, and thus did not change
      assert(sDeref2_bChanged)

      assert(s2.readValueOnce.readValueOnce.readValueOnce == 0)
    }

    test("list Of Signals Section") {
      val tick    = Evt[Unit]()
      val count   = tick.count()
      val doubled = count.map(_ * 2)
      val mod2    = count.map(_ % 2)

      val listOfSignals: Signal[List[Signal[Int]]] = Signal.static() { _ => List(doubled, count) }
      val selected: Signal[Signal[Int]]            = Signal { listOfSignals.value.apply(mod2()) }
      val dereferenced                             = selected.flatten

      var dereferencedChanged = false
      dereferenced.changed observe { _ => dereferencedChanged = true }

      tick.fire()
      assert(count.readValueOnce == 1)
      assert(doubled.readValueOnce == 2)
      assert(mod2.readValueOnce == 1)
      assert(selected.readValueOnce == count)
      assert(dereferencedChanged)
      dereferencedChanged = false
      assert(dereferenced.readValueOnce == 1)

      tick.fire()
      assert(count.readValueOnce == 2)
      assert(doubled.readValueOnce == 4)
      assert(mod2.readValueOnce == 0)
      assert(selected.readValueOnce == doubled)
      assert(dereferencedChanged)
      dereferencedChanged = false
      assert(dereferenced.readValueOnce == 4)
    }

    test("event of options") {
      val someInput = Evt[Option[String]]()
      val flat      = someInput.flatten
      val res       = flat.hold()
      var count     = 0

      res.observe { _ => count += 1 }

      assert(count == 0)

      someInput.fire(Some("Hello"))
      assert(count == 1, "first some")
      assert(res.readValueOnce == "Hello", "flatten some")

      someInput.fire(None)
      assert(count == 1, "first none")
      assert(res.readValueOnce == "Hello", "flatten none")

      someInput.fire(Some("World"))
      assert(count == 2, "second some")
      assert(res.readValueOnce == "World", "flatten some again")

    }

    test("flatten from future type inference") {
      val joined = Evt[String]()
      import scala.concurrent.ExecutionContext.Implicits.global
      val res = (joined.map(str => engine.Signal.fromFuture(Future.successful(str)))
        .hold(Signal { "unknown" })).flatten

      joined.fire("test")

      assert(res.readValueOnce == "test")
    }

  }
}
