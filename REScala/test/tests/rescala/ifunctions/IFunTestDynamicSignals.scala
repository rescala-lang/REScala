package tests.rescala.ifunctions

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.turns.Engines.default
import rescala.{Event, Evt, Signal, Var}


class IFunTestDynamicSignals extends AssertionsForJUnit with MockitoSugar {

  /* count */
  @Test def count_theInitialValueIsSetCorrectly(): Unit = {
    val e = new Evt[Int]()
    val s: Signal[Int] = e.count
    assert(s.now == 0)
  }

  @Test def count_theResultSignalIncreasesWhenEventsOccur(): Unit = {
    val e = new Evt[Int]()
    val s: Signal[Int] = e.count
    e(1)
    e(1)
    assert(s.now == 2)
  }

  /* toggle */
  @Test def toggle_theInitialValueIsSetCorrectly(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val v2 = Var(11)
    val s2 = v2.map(_ + 1)
    val s = e.toggle(s1, s2)

    assert(s.now == 2)
  }

  @Test def toggle_theEventSwitchesTheSignal(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val v2 = Var(11)
    val s2 = v2.map(_ + 1)
    val s = e.toggle(s1, s2)

    assert(s.now == 2)
    e(1)
    assert(s.now == 12)
    v2.set(12)
    assert(s.now == 13)
    v1.set(2)
    assert(s.now == 13)
    e(1)
    v1.set(3)
    assert(s.now == 4)
    v2.set(13)
    assert(s.now == 4)

  }

  /* snapshot */
  @Test def snapshot_theInitialValueIsSetCorrectly(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s = e.snapshot(s1)

    assert(s.now == 2)
  }

  @Test def snapshot_takesASnapshotWhenTheEventOccurs(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s = e.snapshot(s1)

    e(1)
    assert(s.now == 2)

    v1.set(2)
    assert(s.now == 2)
    e(1)
    assert(s.now == 3)
  }

  /* delay[T](signal: Signal[T], n: Int): Signal[T] */
  @Test def delay1_theInitialValueIsSetCorrectly(): Unit = {
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s = s1.delay(3)

    assert(s.now == 2)
  }

  @Test def delay1_takesASnapshotWhenTheEventOccurs(): Unit = {
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s = s1.delay(3)

    // Initially remains the same for n times
    v1.set(2)
    assert(s.now == 2)
    v1.set(3)
    assert(s.now == 2)
    v1.set(4)
    assert(s.now == 2)

    // Now starts changing
    v1.set(5)
    assert(s.now == 3)
    v1.set(6)
    assert(s.now == 4)
  }

  /* switchTo */
  @Test def switchTo_theInitialValueIsSetToTheSignal(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s2 = e.switchTo(s1)

    assert(s2.now == 2)
    v1.set(2)
    assert(s2.now == 3)
  }

  @Test def switchTo_theEventSwitchesTheValueToTheValueOfTheEvent(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s2 = e.switchTo(s1)

    e(1)
    assert(s2.now == 1)
    e(100)
    assert(s2.now == 100)
    v1.set(2)
    assert(s2.now == 100)
  }

  /* switchOnce */
  @Test def switchOnce_theInitialValueIsSetToTheSignal(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map(_ + 1)
    val s2 = v2.map(_ + 1)
    val s3 = e.switchOnce(s1, s2)

    assert(s3.now == 1)
    v1.set(1)
    assert(s3.now == 2)
  }

  @Test def switchOnce_theEventSwitchesTheValueToTheValueOfTheOtherSignal(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map(_ + 1)
    val s2 = v2.map(_ + 1)
    val s3 = e.switchOnce(s1, s2)

    e(1)
    assert(s3.now == 11)
    e(2)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* reset */
  @Test def reset_TheInitialValueOfTheSignalIsGivenByInitAndTheFactory(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map(_ + 1)
    val s2 = v2.map(_ + 1)

    def factory(x: Int) = x % 2 match {
      case 0 => s1
      case 1 => s2
    }
    val s3 = e.reset(100)(factory)

    assert(s3.now == 1)
    v1.set(1)
    assert(s3.now == 2)

  }

  @Test def reset_TheValueOfTheSignalIsGivenByTheEventAndTheFactory(): Unit = {
    val e = new Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map(_ + 1)
    val s2 = v2.map(_ + 1)

    def factory(x: Int) = x % 2 match {
      case 0 => s1
      case 1 => s2
    }

    val s3 = e.reset(100)(factory)

    //assert(s3.get == 1)
    v1.set(1)
    assert(s3.now == 2)
    e(101)
    assert(s3.now == 11)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* change */
  @Test def change_isNotTriggeredOnCreation(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e = s1.change
    e += { x => test += 1 }

    assert(test == 0)
  }

  @Test def change_isTriggeredWhenTheSignalChanges(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e = s1.change
    e += { x => test += 1 }

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  @Test def change_theValueOfTheEventReflectsTheChangeInTheSignal(): Unit = {
    var test = (0, 0)
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e = s1.change
    e += { x => test = x }

    v1 set 2
    assert(test === ((2, 3)))
    v1 set 3
    assert(test === ((3, 4)))
  }

  /* changed */
  @Test def changed_isNotTriggeredOnCreation(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    assert(test == 0)
  }

  @Test def changed_isTriggeredWhenTheSignalChanges(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
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
    val s1 = v1.map(_ + 1)
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
    val s1 = v1.map(_ + 1)
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => { test += 1 })

    assert(test == 0)
  }

  @Test def changedTo_isTriggeredWhenTheSignalHasTheGivenValue(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e: Event[Unit] = s1.changedTo(3)
    e += ((x: Unit) => { test += 1 })

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 1)
  }

}
