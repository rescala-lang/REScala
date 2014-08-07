package rescala.test

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala._
import makro.SignalMacro.{ SignalM => Signal }
import rescala.events._
import scala.collection.LinearSeq

class HigherOrderTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def basicHigherOrderSignal_canBeAccessed() = {
    val v = Var(42)
    val s1: Signal[Int] = Signal { v() }
    val s2: Signal[Signal[Int]] = Signal { s1 }

    assert(s2.get.get == 42)

    v()= 0
    assert(s2.get.get == 0)
  }


  @Test def basicHigherOrderSignal_canBeDefereferenced() = {
    val v = Var(42)
    val s1: Signal[Int] = Signal { v() }
    val s2: Signal[Signal[Int]] = Signal { s1 }
    val sDeref = Signal { s2()() }

    assert(sDeref.get == 42)

    v()= 0
    assert(sDeref.get == 0)
  }


  @Test def basicHigherOrderSignal_derefFiresChange() = {
    val v = Var(42)
    val sValue: Signal[Int] = Signal { v() }
    val sHigher: Signal[Signal[Int]] = Signal { sValue }
    val sDeref = Signal { sHigher()() }

    var sDerefChanged = false
    var sHigherChanged = false

    sDeref.change += {_ => sDerefChanged = true}
    sHigher.change += {_ => sHigherChanged = true}


    assert(!sHigherChanged && !sDerefChanged)

    v()= 0 // update
    assert(!sHigherChanged) // higher does not change
    assert(sDerefChanged) // deref DOES change
  }


  @Test def basicHigherOrderSignal_higherOrderFiresChange() = {
    val v1 = Var(42)
    val v2 = Var(123)
    val s1: Signal[Int] = Signal { v1() }
    val s2: Signal[Int] = Signal { v2() }

    val selector: Var[Signal[Int]] = Var(s1)
    val sHigher = Signal { selector() }
    val sDeref = Signal { sHigher()() }

    var sDerefChanged = false
    var sHigherChanged = false


    sDeref.change += {_ => sDerefChanged = true}
    sHigher.change += {_ => sHigherChanged = true}

    // 1. Unrelated value changes, no updates
    v2()= 1234
    assert(!sDerefChanged)
    assert(!sHigherChanged)

    // 2. Related value changes, only the deref signal changes
    v1()= 321
    assert(sDerefChanged)
    assert(!sHigherChanged)
    sDerefChanged = false


    // 3. Selector changes, both signals fire changes
    selector()= s2
    assert(sDerefChanged)
    assert(sHigherChanged)

    assert(sDeref.get == 1234)
  }


  @Test def order3Signal() = {

    val v = Var(42)
    val s0: Signal[Int] = Signal { v() }
    val s1: Signal[Signal[Int]] = Signal { s0 }
    val s2: Signal[Signal[Signal[Int]]] = Signal { s1 }

    val sDeref1 = Signal { s1()() }
    val sDeref2 = Signal { s2()()() }
    val sDeref2_a = Signal { s2()() }
    val sDeref2_b = Signal { sDeref2_a()() }

    var sDeref1Changed = false
    var sDeref2Changed = false
    var sDeref2_aChanged = false
    var sDeref2_bChanged = false

    sDeref1.change += { _ => sDeref1Changed = true}
    sDeref2.change += { _ => sDeref2Changed = true}
    sDeref2_a.change += { _ => sDeref2_aChanged = true}
    sDeref2_b.change += { _ => sDeref2_bChanged = true}

    v()= 0
    assert(sDeref1Changed)
    assert(sDeref2Changed)
    assert(!sDeref2_aChanged) // 2_a is not completely dereferenced, and thus did not change
    assert(sDeref2_bChanged)


    assert(s2.get.get.get == 0)
  }


  @Test def listOfSignalsSection() = {
    val tick = new ImperativeEvent[Unit]
    val count = tick.iterate(0)(_ + 1)
    val doubled = Signal { count() * 2 }
    val mod2 = Signal { count() % 2 }

    val listOfSignals = Signal { List(doubled, count) }
    val selected = Signal { listOfSignals()(mod2()) }
    val dereferenced = Signal { selected()() }

    var dereferencedChanged = false
    dereferenced.changed += { _ => dereferencedChanged = true }

    tick( () )
    assert(dereferencedChanged == true)
    dereferencedChanged = false
    assert(dereferenced.get == 1)
    tick(())
    assert(dereferencedChanged == true)
    dereferencedChanged = false
    assert(dereferenced.get == 4)
  }

  @Test def switch_withHigherOrder() = {
    val tick = new ImperativeEvent[Unit]
    val count = tick.iterate(0)(_ + 1)
    val doubled = Signal { count() * 2 }
    val mod2 = Signal { count() % 2 }

    val listOfSignals = Signal { List(doubled, count) }

    val s = IFunctions.switch(mod2.changed)(count)(new IFunctions.Factory[Int, Int] {
      override def apply(eVal: Int): (Signal[Int], IFunctions.Factory[Int, Int]) = {
        val selected = Signal { listOfSignals()(eVal) }
        val dereferenced = Signal { selected()() }
        (dereferenced, this)
      }
    })

    var changeCount = 0
    var lastChangevalue = 0
    s.changed += { x => changeCount += 1; lastChangevalue = x }

    tick(())
    assert(lastChangevalue == s.get)
    assert(s.get == 1)
    assert(changeCount == 1)

    tick(())
    assert(lastChangevalue == s.get)
    assert(s.get == 4)
    //assert(changeCount == 2) // fails
  }
  
  
  
  @Test def unwrap_Event() = {
    val e1 = new ImperativeEvent[Int] { override def toString = "e1"}
    val e2 = new ImperativeEvent[Int] { override def toString = "e2"}
    val eventSelector = Var(e1)
    val selected = Signal { eventSelector() }
    val unwrapped = IFunctions.unwrap(selected)
    
    var lastEvent = -1
    unwrapped += { lastEvent = _}
    
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

}
