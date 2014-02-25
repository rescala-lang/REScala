package react.test.ifunctions
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react._
import macro.SignalMacro.{ SignalM => Signal }
import react.events._
import scala.collection.LinearSeq

class IFunTestDynamicSignals extends AssertionsForJUnit with MockitoSugar {

  /* fold */
  @Test def fold_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val f = (x: Int, y: Int) => (x + y)
    val s: Signal[Int] = e.fold(10)(f)
    assert(s.getValue == 10)
  }

  @Test def fold_theResultSignalIncreasesWhenEventsOccur() {
    val e = new ImperativeEvent[Int]()
    val f = (x: Int, y: Int) => (x + y)
    val s: Signal[Int] = e.fold(10)(f)
    e(1)
    e(1)
    assert(s.getValue == 12)
  }

  /* iterate */
  @Test def iterate_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val f = (x: Int) => (x)
    val s: Signal[Int] = e.iterate(10)(f)
    assert(s.getValue == 10)
  }

  @Test def iterate_theFunctionisExecutedEveryTimeTheEventFires() {
    var test: Int = 0
    val e = new ImperativeEvent[Int]()
    val f = (x: Int) => { test += 1; x }
    val s: Signal[Int] = e.iterate(10)(f)
    e(1)
    assert(test == 1)
    e(2)
    assert(test == 2)
    e(1)
    assert(test == 3)
  }

  // TODO: does it make sense ?
  @Test def iterate_theParameterIsAlwaysTheInitValue() {
    var test: Int = 0
    val e = new ImperativeEvent[Int]()
    val f = (x: Int) => { test = x; x + 1 }
    val s: Signal[Int] = e.iterate(10)(f)
    e(1)
    assert(test == 10)
    e(2)
    assert(test == 11)
    e(1)
    assert(test == 12)
  }

  @Test def iterate_theResultSignalIsNeverChanged() {
    var test: Int = 0
    val e = new ImperativeEvent[Int]()
    val f = (x: Int) => { test += x; x } // TODO: I would expect that f is A=>Unit, since the result is never used
    val s: Signal[Int] = e.iterate(10)(f)
    e(1)
    assert(s.getValue == 10)
    e(2)
    assert(s.getValue == 10)
    e(1)
    assert(s.getValue == 10)
  }

  /* count */
  @Test def count_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.count
    assert(s.getValue == 0)
  }

  @Test def count_theResultSignalIncreasesWhenEventsOccur() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.count
    e(1)
    e(1)
    assert(s.getValue == 2)
  }

  /* latest */
  @Test def latest_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.latest(10)

    assert(s.getValue == 10)
  }

  @Test def latest_theFunctionisExecutedEveryTimeTheEventFires() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.latest(10)

    e(1)
    assert(s.getValue == 1)
    e(2)
    assert(s.getValue == 2)
    e(1)
    assert(s.getValue == 1)
  }

  /* latestOption */
  @Test def latestOption_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Option[Int]] = IFunctions.latestOption(e)

    assert(s.getValue == None)
  }

  @Test def latestOption_theFunctionisExecutedEveryTimeTheEventFires() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Option[Int]] = IFunctions.latestOption(e)

    e(1)
    assert(s.getValue == Option(1))
    e(2)
    assert(s.getValue == Option(2))
    e(1)
    assert(s.getValue == Option(1))
  }

  /* last */
  @Test def last_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.getValue == List())
  }

  @Test def last_collectsTheLastNEvents() {
    val e = new ImperativeEvent[Int]()
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.getValue == LinearSeq())
    e(1)
    assert(s.getValue == LinearSeq(1))
    e(2)
    assert(s.getValue == LinearSeq(1, 2))

    e(3); e(4); e(5)
    assert(s.getValue == LinearSeq(1, 2, 3, 4, 5))
    e(6)
    assert(s.getValue == LinearSeq(2, 3, 4, 5, 6))
  }

  /* list */
  @Test def list_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val s = e.list

    assert(s.getValue == List())
  }

  @Test def list_theFunctionisExecutedEveryTimeTheEventFires() {
    val e = new ImperativeEvent[Int]()
    val s = e.list

    assert(s.getValue == List())
    e(1)
    assert(s.getValue == List(1))
    e(2)
    assert(s.getValue == List(2, 1))

    e(3); e(4); e(5); e(6)
    assert(s.getValue == List(6, 5, 4, 3, 2, 1))
  }

  /* toggle */
  @Test def toggle_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1: Signal[Int] = Signal { v1() + 1 }
    val v2 = Var(11)
    val s2 = Signal { v2() + 1 }
    val s = e.toggle(s1, s1)

    assert(s.getValue == 2)
  }

  @Test def toggle_theEventSwitchesTheSignal() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val v2 = Var(11)
    val s2 = Signal { v2() + 1 }
    val s = e.toggle(s1, s2)

    assert(s.getValue == 2)
    e(1)
    assert(s.getValue == 12)
    v2.setVal(12)
    assert(s.getValue == 13)
    v1.setVal(2)
    assert(s.getValue == 13)
    e(1)
    v1.setVal(3)
    assert(s.getValue == 4)
    v2.setVal(13)
    assert(s.getValue == 4)

  }

  /* snapshot */
  @Test def snapshot_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = e.snapshot(s1)

    assert(s.getValue == 2)
  }

  @Test def snapshot_takesASnapshotWhenTheEventOccurs() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = e.snapshot(s1)

    e(1)
    assert(s.getValue == 2)

    v1.setVal(2)
    assert(s.getValue == 2)
    e(1)
    assert(s.getValue == 3)
  }

  /* delay[T](e: Event[T], init: T, n: Int): Signal[T] */
  @Test def delay_theInitialValueIsSetCorrectly() {
    val e = new ImperativeEvent[Int]()
    val s = e.delay(0, 3)

    assert(s.getValue == 0)
  }

  @Test def delay_takesASnapshotWhenTheEventOccurs() {
    val e = new ImperativeEvent[Int]()
    val s = e.delay(0, 3)

    // Initially remains the same for n times
    e(1)
    assert(s.getValue == 0)
    e(2)
    assert(s.getValue == 0)
    e(3)
    assert(s.getValue == 0)

    // Now starts changing
    e(4)
    assert(s.getVal == 1)
    e(5)
    assert(s.getVal == 2)
    e(6)
    assert(s.getVal == 3)

  }

  /* delay[T](signal: Signal[T], n: Int): Signal[T] */
  @Test def delay1_theInitialValueIsSetCorrectly() {
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = IFunctions.delay(s1, 3)

    assert(s.getVal == 2)
  }

  @Test def delay1_takesASnapshotWhenTheEventOccurs() {
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = IFunctions.delay(s1, 3)

    // Initially remains the same for n times
    v1.setVal(2)
    assert(s.getVal == 2)
    v1.setVal(3)
    assert(s.getVal == 2)
    v1.setVal(4)
    assert(s.getVal == 2)

    // Now starts changing
    v1.setVal(5)
    assert(s.getVal == 3)
    v1.setVal(6)
    assert(s.getVal == 4)

  }

  /* switchTo */
  @Test def switchTo_theInitialValueIsSetToTheSignal() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s2 = IFunctions.switchTo(e, s1)

    assert(s2.getVal == 2)
    v1.setVal(2)
    assert(s2.getVal == 3)
  }

  @Test def switchTo_theEventSwitchesTheValueToTheValueOfTheEvent() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s2 = IFunctions.switchTo(e, s1)

    e(1)
    assert(s2.getVal == 1)
    e(100)
    assert(s2.getVal == 100)
    v1.setVal(2)
    assert(s2.getVal == 100)
  }

  /* switchOnce */
  @Test def switchOnce_theInitialValueIsSetToTheSignal() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = Signal { v1() + 1 }
    val s2 = Signal { v2() + 1 }
    val s3 = IFunctions.switchOnce(e, s1, s2)

    assert(s3.getVal == 1)
    v1.setVal(1)
    assert(s3.getVal == 2)
  }

  @Test def switchOnce_theEventSwitchesTheValueToTheValueOfTheOtherSignal() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = Signal { v1() + 1 }
    val s2 = Signal { v2() + 1 }
    val s3 = IFunctions.switchOnce(e, s1, s2)

    e(1)
    assert(s3.getVal == 11)
    e(2)
    v2.setVal(11)
    assert(s3.getVal == 12)
  }

  @Test def switch_canImplementSnapshot() {
    def snapshot[T, E](s: Signal[T])(e: Event[E]): Signal[T] = {
      val init = Signal(s.getValue)
      IFunctions.switch(e)(init)(new IFunctions.Factory[E, T] {
        override def apply(eVal: E): (Signal[T], IFunctions.Factory[E, T]) = {
          val freeze = s.getVal
          (Signal(freeze), this)
        }
      })
    }
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = snapshot(s1)(e)

    e(1)
    assert(s.getValue == 2)

    v1.setVal(2)
    assert(s.getValue == 2)
    e(1)
    assert(s.getValue == 3)

  }

  /* reset */
  @Test def reset_TheInitialValueOfTheSignalIsGivenByInitAndTheFactory() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = Signal { v1() + 1 }
    val s2 = Signal { v2() + 1 }

    def factory(x: Int) = x % 2 match {
      case 0 => s1
      case 1 => s2
    }
    val s3 = IFunctions.reset(e, 100)(factory)

    assert(s3.getVal == 1)
    v1.setVal(1)
    assert(s3.getVal == 2)

  }

  @Test def reset_TheValueOfTheSignalIsGivenByTheEventAndTheFactory() {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = Signal { v1() + 1 }
    val s2 = Signal { v2() + 1 }

    def factory(x: Int) = x % 2 match {
      case 0 => s1
      case 1 => s2
    }

    val s3 = IFunctions.reset(e, 100)(factory)

    //assert(s3.getVal == 1)
    v1.setVal(1)
    assert(s3.getVal == 2)
    e(101)
    assert(s3.getVal == 11)
    v2.setVal(11)
    assert(s3.getVal == 12)
  }

  /* lift */
  @Test def lift_createsAFunctionThatWorksWithSignals() {
    val v = Var(1)
    val s1 = Signal { v() + 1 }
    def f(x: Int): Int = x + 1

    val lifted_f = IFunctions.lift(f)
    val s2 = lifted_f(s1)

    assert(s2.getVal == 3)
    v.setVal(2)
    assert(s2.getVal == 4)
  }

  /* change */
  @Test def change_isNotTriggeredOnCreation() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[(Int, Int)] = s1.change
    e += ((x: (Int, Int)) => { test += 1 })

    assert(test == 0)
  }

  @Test def change_isTriggeredWhenTheSignalChanges() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[(Int, Int)] = s1.change
    e += ((x: (Int, Int)) => { test += 1 })

    v1 setVal 2
    assert(test == 1)
    v1 setVal 3
    assert(test == 2)
  }

  @Test def change_theValueOfTheEventReflectsTheChangeInTheSignal() {
    var test = (0, 0)
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[(Int, Int)] = s1.change
    e += ((x: (Int, Int)) => { test = x })

    v1 setVal 2
    assert(test == (null, 3))
    v1 setVal 3
    assert(test == (3, 4))
  }

  /* changed */
  @Test def changed_isNotTriggeredOnCreation() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    assert(test == 0)
  }

  @Test def changed_isTriggeredWhenTheSignalChanges() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    v1 setVal 2
    assert(test == 1)
    v1 setVal 3
    assert(test == 2)
  }

  @Test def changed_theValueOfTheEventReflectsTheChangeInTheSignal() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test = x })

    v1 setVal 2
    assert(test == 3)
    v1 setVal 3
    assert(test == 4)
  }

  /* changedTo */
  @Test def changedTo_isNotTriggeredOnCreation() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => { test += 1 })

    assert(test == 0)
  }

  @Test def changedTo_isTriggeredWhenTheSignalHasTheGivenValue() {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Unit] = s1.changedTo(3)
    e += ((x: Unit) => { test += 1 })

    v1 setVal 2
    assert(test == 1)
    v1 setVal 3
    assert(test == 1)
  }

  @Test def xxxx = {

    val a = Var(3)
    val b = Var(Signal(a()))
    val c = SignalSynt[Int](b) { (x: SignalSynt[Int]) => b(x)(x) }
    //println(c.getVal) //outputs 3
    a() = 4
    //println(c.getVal) //outputs 4
    b() = Signal(5)
    //println(c.getVal) //outputs 4

  }

  @Test def higherOrderSignals = {
    val tick = new ImperativeEvent[Unit]
    val count = tick.iterate(0)(_ + 1)
    val doubled = Signal { count() * 2 }
    val mod2 = Signal { count() % 2 }

    val listOfSignals = Signal { List(doubled, count) }
    val selected = Signal { listOfSignals()(mod2()) }
    val dereferenced = Signal { selected()() }

    var dereferencedChanged = false
    dereferenced.changed += { _ => dereferencedChanged = true }

    tick()
    assert(dereferencedChanged == true)
    dereferencedChanged = false
    assert(dereferenced.getValue == 1)
    tick()
    assert(dereferencedChanged == true)
    dereferencedChanged = false
    assert(dereferenced.getValue == 4)
  }

  @Test def switch_withHigherOrder = {
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
    s.changed += {x => changeCount += 1; lastChangevalue = x}

    tick()
    assert(lastChangevalue == s.getValue)
    assert(s.getValue == 1)
    assert(changeCount == 1)

    tick()
    assert(lastChangevalue == s.getValue)
    assert(s.getValue == 4)
    //assert(changeCount == 2) // fails
  }
}





