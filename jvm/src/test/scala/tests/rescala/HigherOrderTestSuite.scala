package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.Signals


object HigherOrderTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class HigherOrderTestSuite[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit  {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Event, Evt, Signal, Var, dynamic}


  @Test def basicHigherOrderSignal_canBeAccessed(): Unit = {
    val v = Var(42)
    val s1: Signal[Int] = v.map(identity)
    val s2: Signal[Signal[Int]] = dynamic() { t => s1 }

    assert(s2.now.now == 42)

    v.set(0)
    assert(s2.now.now == 0)
  }

  @Test def basicHigherOrderSignal_canBeDefereferenced(): Unit = {
    val v = Var(42)
    val s1: Signal[Int] = v.map(identity)
    val s2: Signal[Signal[Int]] = dynamic() { t => s1 }
    val sDeref = s2.flatten

    assert(sDeref.now == 42)

    v.set(0)
    assert(sDeref.now == 0)
  }


  @Test def basicHigherOrderSignal_derefFiresChange(): Unit = {
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


  @Test def basicHigherOrderSignal_higherOrderFiresChange(): Unit = {
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


  @Test def order3Signal(): Unit = {

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


  @Test def listOfSignalsSection(): Unit = {
    val tick = Evt[Unit]
    val count = tick.iterate(0)(_ + 1)
    val doubled = count.map(_ * 2)
    val mod2 = count.map(_ % 2)

    val listOfSignals: Signal[List[Signal[Int]]] = Signals.static() { t => List(doubled, count) }
    val selected: Signal[Signal[Int]] = dynamic(listOfSignals, mod2) { t => listOfSignals(t)(mod2(t)) }
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


  @Test def unwrap_Event(): Unit = {
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

  @Test def dynamicLevel(): Unit = {
    val v1 = Var(1)

    val derived = v1.map(identity)

    val level1 = v1.map(_ + 1)
    val level2 = level1.map(_ + 1)
    val level3 = level2.map(_ + 1)


    val combined = dynamic() { t => if (v1(t) == 10) level3(t) else derived(t) }

    var log = List[Int]()

    combined.changed += (log ::= _)

    v1.set(10)
    assert(log == List(13))
    v1.set(1)
    assert(log == List(1, 13))


    val higherOrder = dynamic() { t => if (v1(t) == 10) level3 else derived }
    val flattened = higherOrder.flatten

    var higherOrderLog = List[Int]()

    flattened.changed += (higherOrderLog ::= _)

    v1.set(10)
    assert(higherOrderLog == List(13))
    v1.set(1)
    assert(higherOrderLog == List(1, 13))
    assert(log == List(1, 13, 1, 13))
  }

  @Test def wrappedEvent(): Unit = {
    val e1 = Evt[Int]
    val condition = e1.latest(-1)
    val level1Event = e1.map(_ => "level 1")
    val level2Event = level1Event.map(_ => "level 2")
    val dynamicSignal = dynamic() { t => if (condition(t) == 1) level1Event else level2Event }

    val unwrapped = dynamicSignal.flatten

    var log = List[String]()
    unwrapped += (log ::= _)

    e1.apply(0)
    assert(log == List("level 2"))
    e1.apply(1)
    assert(log == List("level 1", "level 2"))
  }

  @Test def wrappedEventSameLevel(): Unit = {
    val e1 = Evt[Int]
    val level2Condition = e1.latest(-1).map(identity)
    val level1EventA = e1.map(_ => "A")
    val level1EventB = e1.map(_ => "B")
    val dynamicSignal = dynamic() { t => if (level2Condition(t) == 1) level1EventA else level1EventB }

    val unwrapped = dynamicSignal.flatten

    var log = List[String]()
    unwrapped += (log ::= _)

    e1.apply(0)
    assert(log == List("B"))
    e1.apply(1)
    assert(log == List("A", "B"))
  }


  @Test def flattenEvents(): Unit = {
    val e1 = Evt[Event[Int]]
    val f1 = e1.flatten
    val res = f1.log()
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

}
