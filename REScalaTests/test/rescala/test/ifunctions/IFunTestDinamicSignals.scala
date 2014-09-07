package rescala.test.ifunctions
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala._
import rescala.events._
import rescala.makro.SignalMacro.{SignalM => Signal}

import scala.collection.LinearSeq

class IFunTestDynamicSignals extends AssertionsForJUnit with MockitoSugar {

  /* fold */
  @Test def fold_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    assert(s.get == 10)
  }

  @Test def fold_theResultSignalIncreasesWhenEventsOccur(): Unit = {
    val e = new ImperativeEvent[Int]()
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    e(1)
    e(1)
    assert(s.get == 12)
  }

  /* iterate */
  @Test def iterate_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val f = (x: Int) => x
    val s: Signal[Int] = e.iterate(10)(f)
    assert(s.get == 10)
  }

  @Test def iterate_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
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
  @Test def iterate_theParameterIsAlwaysTheInitValue(): Unit = {
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

  @Test def iterate_theResultSignalIsNeverChanged(): Unit = {
    var test: Int = 0
    val e = new ImperativeEvent[Int]()
    val f = (x: Int) => { test += x; x } // TODO: I would expect that f is A=>Unit, since the result is never used
    val s: Signal[Int] = e.iterate(10)(f)
    e(1)
    assert(s.get == 10)
    e(2)
    assert(s.get == 10)
    e(1)
    assert(s.get == 10)
  }

  /* count */
  @Test def count_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.count
    assert(s.get == 0)
  }

  @Test def count_theResultSignalIncreasesWhenEventsOccur(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.count
    e(1)
    e(1)
    assert(s.get == 2)
  }

  /* latest */
  @Test def latest_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.latest(10)

    assert(s.get == 10)
  }

  @Test def latest_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Int] = e.latest(10)

    e(1)
    assert(s.get == 1)
    e(2)
    assert(s.get == 2)
    e(1)
    assert(s.get == 1)
  }

  /* latestOption */
  @Test def latestOption_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Option[Int]] = IFunctions.latestOption(e)

    assert(s.get == None)
  }

  @Test def latestOption_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[Option[Int]] = IFunctions.latestOption(e)

    e(1)
    assert(s.get == Option(1))
    e(2)
    assert(s.get == Option(2))
    e(1)
    assert(s.get == Option(1))
  }

  /* last */
  @Test def last_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.get == List())
  }

  @Test def last_collectsTheLastNEvents(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.get == LinearSeq())
    e(1)
    assert(s.get == LinearSeq(1))
    e(2)
    assert(s.get == LinearSeq(1, 2))

    e(3); e(4); e(5)
    assert(s.get == LinearSeq(1, 2, 3, 4, 5))
    e(6)
    assert(s.get == LinearSeq(2, 3, 4, 5, 6))
  }

  /* list */
  @Test def list_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s = e.list()

    assert(s.get == List())
  }

  @Test def list_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s = e.list()

    assert(s.get == List())
    e(1)
    assert(s.get == List(1))
    e(2)
    assert(s.get == List(2, 1))

    e(3); e(4); e(5); e(6)
    assert(s.get == List(6, 5, 4, 3, 2, 1))
  }

  /* toggle */
  @Test def toggle_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1: Signal[Int] = Signal { v1() + 1 }
    val v2 = Var(11)
    val s2 = Signal { v2() + 1 }
    val s = e.toggle(s1, s1)

    assert(s.get == 2)
  }

  @Test def toggle_theEventSwitchesTheSignal(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val v2 = Var(11)
    val s2 = Signal { v2() + 1 }
    val s = e.toggle(s1, s2)

    assert(s.get == 2)
    e(1)
    assert(s.get == 12)
    v2.set(12)
    assert(s.get == 13)
    v1.set(2)
    assert(s.get == 13)
    e(1)
    v1.set(3)
    assert(s.get == 4)
    v2.set(13)
    assert(s.get == 4)

  }

  /* snapshot */
  @Test def snapshot_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = e.snapshot(s1)

    assert(s.get == 2)
  }

  @Test def snapshot_takesASnapshotWhenTheEventOccurs(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = e.snapshot(s1)

    e(1)
    assert(s.get == 2)

    v1.set(2)
    assert(s.get == 2)
    e(1)
    assert(s.get == 3)
  }

  /* delay[T](e: Event[T], init: T, n: Int): Signal[T] */
  @Test def delay_theInitialValueIsSetCorrectly(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s = e.delay(0, 3)

    assert(s.get == 0)
  }

  @Test def delay_takesASnapshotWhenTheEventOccurs(): Unit = {
    val e = new ImperativeEvent[Int]()
    val s = e.delay(0, 3)

    // Initially remains the same for n times
    e(1)
    assert(s.get == 0)
    e(2)
    assert(s.get == 0)
    e(3)
    assert(s.get == 0)

    // Now starts changing
    e(4)
    assert(s.get == 1)
    e(5)
    assert(s.get == 2)
    e(6)
    assert(s.get == 3)

  }

  /* delay[T](signal: Signal[T], n: Int): Signal[T] */
  @Test def delay1_theInitialValueIsSetCorrectly(): Unit = {
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = IFunctions.delay(s1, 3)

    assert(s.get == 2)
  }

  @Test def delay1_takesASnapshotWhenTheEventOccurs(): Unit = {
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = IFunctions.delay(s1, 3)

    // Initially remains the same for n times
    v1.set(2)
    assert(s.get == 2)
    v1.set(3)
    assert(s.get == 2)
    v1.set(4)
    assert(s.get == 2)

    // Now starts changing
    v1.set(5)
    assert(s.get == 3)
    v1.set(6)
    assert(s.get == 4)

  }

  /* switchTo */
  @Test def switchTo_theInitialValueIsSetToTheSignal(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s2 = IFunctions.switchTo(e, s1)

    assert(s2.get == 2)
    v1.set(2)
    assert(s2.get == 3)
  }

  @Test def switchTo_theEventSwitchesTheValueToTheValueOfTheEvent(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 =  Var(1)
    val s1 = Signal{ v1() + 1 }
    val s2 = IFunctions.switchTo(e,s1)

    e(1)
    assert(s2.get == 1)
    e(100)
    assert(s2.get == 100)
    v1.set(2)
    assert(s2.get == 100)
  }

  /* switchOnce */
  @Test def switchOnce_theInitialValueIsSetToTheSignal(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = Signal { v1() + 1 }
    val s2 = Signal { v2() + 1 }
    val s3 = IFunctions.switchOnce(e, s1, s2)

    assert(s3.get == 1)
    v1.set(1)
    assert(s3.get == 2)
  }

  @Test def switchOnce_theEventSwitchesTheValueToTheValueOfTheOtherSignal(): Unit = {
    val e = new ImperativeEvent[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = Signal { v1() + 1 }
    val s2 = Signal { v2() + 1 }
    val s3 = IFunctions.switchOnce(e, s1, s2)

    e(1)
    assert(s3.get == 11)
    e(2)
    v2.set(11)
    assert(s3.get == 12)
  }

  @Test def switch_canImplementSnapshot(): Unit = {
    def snapshot[T, E](s: Signal[T])(e: Event[E]): Signal[T] = {
      val init = Signal(s.get)
      IFunctions.switch(e)(init)(new IFunctions.Factory[E, T] {
        override def apply(eVal: E): (Signal[T], IFunctions.Factory[E, T]) = {
          val freeze = s.get
          (Signal(freeze), this)
        }
      })
    }
    val e = new ImperativeEvent[Int]()
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val s = snapshot(s1)(e)

    e(1)
    assert(s.get == 2)

    v1.set(2)
    assert(s.get == 2)
    e(1)
    assert(s.get == 3)

  }

  /* reset */
  @Test def reset_TheInitialValueOfTheSignalIsGivenByInitAndTheFactory(): Unit = {
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

    assert(s3.get == 1)
    v1.set(1)
    assert(s3.get == 2)

  }

  @Test def reset_TheValueOfTheSignalIsGivenByTheEventAndTheFactory(): Unit = {
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

    //assert(s3.get == 1)
    v1.set(1)
    assert(s3.get == 2)
    e(101)
    assert(s3.get == 11)
    v2.set(11)
    assert(s3.get == 12)
  }

  /* lift */
  @Test def lift_createsAFunctionThatWorksWithSignals(): Unit = {
    val v = Var(1)
    val s1 = Signal { v() + 1 }
    def f(x: Int): Int = x + 1

    val lifted_f = IFunctions.lift(f)
    val s2 = lifted_f(s1)

    assert(s2.get == 3)
    v.set(2)
    assert(s2.get == 4)
  }

  /* change */
  @Test def change_isNotTriggeredOnCreation(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[(Int, Int)] = s1.change
    e += ((x: (Int, Int)) => { test += 1 })

    assert(test == 0)
  }

  @Test def change_isTriggeredWhenTheSignalChanges(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[(Int, Int)] = s1.change
    e += ((x: (Int, Int)) => { test += 1 })

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  @Test def change_theValueOfTheEventReflectsTheChangeInTheSignal(): Unit = {
    var test = (0, 0)
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[(Int, Int)] = s1.change
    e += ((x: (Int, Int)) => { test = x })

    v1 set 2
    assert(test == ((null, 3)))
    v1 set 3
    assert(test == ((3, 4)))
  }

  /* changed */
  @Test def changed_isNotTriggeredOnCreation(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    assert(test == 0)
  }

  @Test def changed_isTriggeredWhenTheSignalChanges(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  @Test def changed_theValueOfTheEventReflectsTheChangeInTheSignal(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test = x })

    v1 set 2
    assert(test == 3)
    v1 set 3
    assert(test == 4)
  }

  /* changedTo */
  @Test def changedTo_isNotTriggeredOnCreation(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => { test += 1 })

    assert(test == 0)
  }

  @Test def changedTo_isTriggeredWhenTheSignalHasTheGivenValue(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = Signal { v1() + 1 }
    val e: Event[Unit] = s1.changedTo(3)
    e += ((x: Unit) => { test += 1 })

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 1)
  }

  @Test def xxxx() = {

    val a = Var(3)
    val b = Var(Signal(a()))
    val c = SignalSynt[Int](b) { (x: SignalSynt[Int]) => b(x)(x) }
    //println(c.get) //outputs 3
    a() = 4
    //println(c.get) //outputs 4
    b() = Signal(5)
    //println(c.get) //outputs 4

  }
}
