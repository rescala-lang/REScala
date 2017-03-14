package tests.rescala


class HigherOrderTestSuite extends RETests {




  allEngines("basic Higher Order Signal can Be Accessed"){ engine => import engine._
    val v = Var(42)
    val s1: Signal[Int] = v.map(identity)
    val s2: Signal[Signal[Int]] = dynamic() { t => s1 }

    assert(s2.now.now == 42)

    v.set(0)
    assert(s2.now.now == 0)
  }

  allEngines("basic Higher Order Signal can Be Defereferenced"){ engine => import engine._
    val v = Var(42)
    val s1: Signal[Int] = v.map(identity)
    val s2: Signal[Signal[Int]] = dynamic() { t => s1 }
    val sDeref = s2.flatten

    assert(sDeref.now == 42)

    v.set(0)
    assert(sDeref.now == 0)
  }


  allEngines("basic Higher Order Signal deref Fires Change"){ engine => import engine._
    val v = Var(42)
    val sValue: Signal[Int] = v.map(identity)
    val sHigher: Signal[Signal[Int]] = dynamic() { t => sValue }
    val sDeref = sHigher.flatten

    var sDerefChanged = false
    var sHigherChanged = false

    sDeref.change += { _ => sDerefChanged = true }
    sHigher.change += { _ => sHigherChanged = true }


    assert(!sHigherChanged && !sDerefChanged)

    v.set(0) // update
    assert(!sHigherChanged) // higher does not change
    assert(sDerefChanged) // deref DOES change
  }


  allEngines("basic Higher Order Signal higher Order Fires Change"){ engine => import engine._
    val v1 = Var(42)
    val v2 = Var(123)
    val s1: Signal[Int] = v1.map(identity)
    val s2: Signal[Int] = v2.map(identity)

    val selector: Var[Signal[Int]] = Var(s1)
    val sHigher = selector.map(identity)
    val sDeref = sHigher.flatten

    var sDerefChanged = false
    var sHigherChanged = false


    sDeref.change += { _ => sDerefChanged = true }
    sHigher.change += { _ => sHigherChanged = true }

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
    selector() = s2
    assert(sDerefChanged)
    assert(sHigherChanged)

    assert(sDeref.now == 1234)
  }


  allEngines("order3 Signal"){ engine => import engine._

    val v = Var(42)
    val s0: Signal[Int] = v.map(identity)
    val s1: Signal[Signal[Int]] = Signals.static() { t => s0 }
    val s2: Signal[Signal[Signal[Int]]] = Signals.static() { t => s1 }

    val sDeref1 = s1.flatten
    val sDeref2 = s2.flatten.flatten
    val sDeref2_a = s2.flatten
    val sDeref2_b = sDeref2_a.flatten

    var sDeref1Changed = false
    var sDeref2Changed = false
    var sDeref2_aChanged = false
    var sDeref2_bChanged = false

    sDeref1.change += { _ => sDeref1Changed = true }
    sDeref2.change += { _ => sDeref2Changed = true }
    sDeref2_a.change += { _ => sDeref2_aChanged = true }
    sDeref2_b.change += { _ => sDeref2_bChanged = true }

    v.set(0)
    assert(sDeref1Changed)
    assert(sDeref2Changed)
    assert(!sDeref2_aChanged) // 2_a is not completely dereferenced, and thus did not change
    assert(sDeref2_bChanged)


    assert(s2.now.now.now == 0)
  }


  allEngines("list Of Signals Section"){ engine => import engine._
    val tick = Evt[Unit]
    val count = tick.iterate(0)(_ + 1)
    val doubled = count.map(_ * 2)
    val mod2 = count.map(_ % 2)

    val listOfSignals: Signal[List[Signal[Int]]] = Signals.static() { t => List(doubled, count) }
    val selected: Signal[Signal[Int]] = Signal { listOfSignals()(mod2()) }
    val dereferenced = selected.flatten

    var dereferencedChanged = false
    dereferenced.changed += { _ => dereferencedChanged = true }

    tick(())
    assert(count.now == 1)
    assert(doubled.now ==2)
    assert(mod2.now == 1)
    assert(selected.now == count)
    assert(dereferencedChanged)
    dereferencedChanged = false
    assert(dereferenced.now == 1)

    tick(())
    assert(count.now == 2)
    assert(doubled.now ==4)
    assert(mod2.now == 0)
    assert(selected.now == doubled)
    assert(dereferencedChanged)
    dereferencedChanged = false
    assert(dereferenced.now == 4)
  }


  allEngines("unwrap  Event"){ engine => import engine._
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val eventSelector = Var(e1)
    val selected = eventSelector.map(identity)
    val unwrapped = selected.flatten

    var lastEvent = -1
    unwrapped += { lastEvent = _ }

    e1(1)
    assert(lastEvent == 1)
    e2(2)
    assert(lastEvent == 1)
    eventSelector() = e2 //select new event source
    e2(3)
    assert(lastEvent == 3)
    e1(4)
    assert(lastEvent == 3)
    e2(5)
    assert(lastEvent == 5)
  }

  allEngines("dynamic Level"){ engine => import engine._
    val v1 = Var(1)

    val derived = v1.map(identity)

    val level1 = v1.map(_ + 1)
    val level2 = level1.map(_ + 1)
    val level3 = level2.map(_ + 1)


    val combined = Signal { if (v1() == 10) level3() else derived() }

    var log = List[Int]()

    combined.changed += (log ::= _)

    v1.set(10)
    assert(log == List(13))
    v1.set(1)
    assert(log == List(1, 13))


    val higherOrder = Signal { if (v1() == 10) level3 else derived }
    val flattened = higherOrder.flatten

    var higherOrderLog = List[Int]()

    flattened.changed += (higherOrderLog ::= _)

    v1.set(10)
    assert(higherOrderLog == List(13))
    v1.set(1)
    assert(higherOrderLog == List(1, 13))
    assert(log == List(1, 13, 1, 13))
  }

  allEngines("wrapped Event"){ engine => import engine._
    val e1 = Evt[Int]
    val condition = e1.latest(-1)
    val level1Event = e1.map(_ => "level 1")
    val level2Event = level1Event.map(_ => "level 2")
    val dynamicSignal = Signal { if (condition() == 1) level1Event else level2Event }

    val unwrapped = dynamicSignal.flatten

    var log = List[String]()
    unwrapped += (log ::= _)

    e1.apply(0)
    assert(log == List("level 2"))
    e1.apply(1)
    assert(log == List("level 1", "level 2"))
  }

  allEngines("wrapped Event Same Level"){ engine => import engine._
    val e1 = Evt[Int]
    val level2Condition = e1.latest(-1).map(identity)
    val level1EventA = e1.map(_ => "A")
    val level1EventB = e1.map(_ => "B")
    val dynamicSignal = Signal { if (level2Condition() == 1) level1EventA else level1EventB }

    val unwrapped = dynamicSignal.flatten

    var log = List[String]()
    unwrapped += (log ::= _)

    e1.apply(0)
    assert(log == List("B"))
    e1.apply(1)
    assert(log == List("A", "B"))
  }


  allEngines("flatten Events"){ engine => import engine._
    val e1 = Evt[Event[Int]]
    val f1 = e1.flatten
    val res = f1.list()
    val e2 = Evt[Int]
    val e3 = Evt[Int]
    e2(10)
    e3(10)

    assert(res.now === Nil)
    e1(e2)
    assert(res.now === Nil)
    e3(10)
    assert(res.now === Nil)
    e2(10)
    assert(res.now === List(10))
    e1(e3)
    assert(res.now === List(10))
    e2(20)
    assert(res.now === List(10))
    e3(30)
    assert(res.now === List(30, 10))

  }


  allEngines("flatten Signal Seq"){ engine => import engine._
    val v = Var.empty[Seq[Signal[Int]]]
    var count = 0
    val v1, v2, v3 = {count += 1 ; Var(count) }
    v.set(List(v1, v2, v3))

    val flat = v.flatten

    assert(flat.now === Seq(1,2,3), "flatten fails")

    v2.set(100)

    assert(flat.now === Seq(1,100,3), "flatten fails 2")

    v.set(List(v3, v2))

    assert(flat.now === Seq(3,100), "flatten fails 3")
  }


  allEngines("flatten Signal Set"){ engine => import engine._
    val v = Var.empty[Set[Var[Int]]]
    var count = 0
    val v1, v2, v3 = {count += 1 ; Var(count) }
    v.set(Set(v1, v2, v3))

    val flat = v.flatten

    assert(flat.now === Set(1,2,3), "flatten fails")

    v2.set(100)

    assert(flat.now === Set(1,100,3), "flatten fails 2")

    v.set(Set(v3, v2))

    assert(flat.now === Set(3,100), "flatten fails 3")
  }


  allEngines("flatten Signal Array"){ engine => import engine._
    val v = Var.empty[Array[Var[Int]]]
    var count = 0
    val v1, v2, v3 = {count += 1 ; Var(count) }
    v.set(Array(v1, v2, v3))

    val flat = v.flatten

    assert(flat.now === Array(1,2,3), "flatten fails")

    v2.set(100)

    assert(flat.now === Array(1,100,3), "flatten fails 2")

    v.set(Array(v3, v2))

    assert(flat.now === Array(3,100), "flatten fails 3")
  }


  allEngines("flatten Signal Option"){ engine => import engine._
    val v = Var(Option.empty[Var[Int]])
    val w = Var(1)

    val flat: Signal[Option[Int]] = v.flatten

    assert(flat.now === None, "flatten fails")

    v.set(Some(w))

    assert(flat.now === Some(1), "flatten fails 2")

    w.set(100)

    assert(flat.now === Some(100), "flatten fails 3")
  }

  allEngines("create changes during reevaluation"){ engine => import engine._
    val v = Var(1)
    val mapped = v.map(_ + 0)

    val sm = Signal { mapped.change.apply() }
    val sd = dynamic() {t => t.depend(mapped.change(t)) }


    //intercept[NoSuchElementException](sm.now)
    assert(sm.now.isEmpty)
    assert(sd.now.isEmpty)

    v.set(2)

    assert(sm.now.get.pair == 1 -> 2)
    assert(sd.now.get.pair == 1 -> 2)

    v.set(3)

    assert(sm.now.get.pair == 2 -> 3)
    assert(sd.now.get.pair == 2 -> 3)


  }

}
