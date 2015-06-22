package tests.rescala.ifunctions

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.turns.{Engine, Turn}
import rescala.{Event, Evt, Signal, Var}
import tests.rescala.JUnitParameters

import scala.collection.LinearSeq


object IFunTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class IFunTest(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine


  /* fold */
  @Test def fold_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    assert(s.now == 10)
  }

  @Test def fold_theResultSignalIncreasesWhenEventsOccur(): Unit = {
    val e = Evt[Int]()
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    e(1)
    e(1)
    assert(s.now == 12)
  }


  /* iterate */
  @Test def iterate_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val f = (x: Int) => x
    val s: Signal[Int] = e.iterate(10)(f)
    assert(s.now == 10)
  }

  @Test def iterate_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    var test: Int = 0
    val e = Evt[Int]()
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
    val e = Evt[Int]()
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
    val e = Evt[Int]()
    val f = (x: Int) => { test += x; x }
    val s: Signal[Int] = e.iterate(10)(f)
    e(1)
    assert(s.now == 10)
    e(2)
    assert(s.now == 10)
    e(1)
    assert(s.now == 10)
  }

  /* latest */
  @Test def latest_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val s: Signal[Int] = e.latest(10)

    assert(s.now == 10)
  }

  @Test def latest_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    val e = Evt[Int]()
    val s: Signal[Int] = e.latest(10)

    e(1)
    assert(s.now == 1)
    e(2)
    assert(s.now == 2)
    e(1)
    assert(s.now == 1)
  }


  /* latestOption */
  @Test def latestOption_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val s: Signal[Option[Int]] = e.latestOption()

    assert(s.now == None)
  }

  @Test def latestOption_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    val e = Evt[Int]()
    val s: Signal[Option[Int]] = e.latestOption()

    e(1)
    assert(s.now == Option(1))
    e(2)
    assert(s.now == Option(2))
    e(1)
    assert(s.now == Option(1))
  }


  /* last */
  @Test def last_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.now == List())
  }

  @Test def last_collectsTheLastNEvents(): Unit = {
    val e = Evt[Int]()
    val s: Signal[LinearSeq[Int]] = e.last(5)


    assert(s.now == LinearSeq())
    e(1)
    assert(s.now == LinearSeq(1))
    e(2)
    assert(s.now == LinearSeq(1, 2))

    e(3)
    e(4)
    e(5)
    assert(s.now == LinearSeq(1, 2, 3, 4, 5))
    e(6)
    assert(s.now == LinearSeq(2, 3, 4, 5, 6))
  }

  /* list */
  @Test def list_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val s = e.list()

    assert(s.now == List())
  }

  @Test def list_theFunctionisExecutedEveryTimeTheEventFires(): Unit = {
    val e = Evt[Int]()
    val s = e.list()

    assert(s.now == List())
    e(1)
    assert(s.now == List(1))
    e(2)
    assert(s.now == List(2, 1))

    e(3)
    e(4)
    e(5)
    e(6)
    assert(s.now == List(6, 5, 4, 3, 2, 1))
  }

  /* toggle */
  @Test def toggle_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val v2 = Var(11)
    val s2 = v2.map { _ + 1 }
    val s = e.toggle(s1, s1)

    assert(s.now == 2)
  }

  @Test def toggle_theEventSwitchesTheSignal(): Unit = {
    val e = Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val v2 = Var(11)
    val s2 = v2.map { _ + 1 }
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
    val e = Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val s = e.snapshot(s1)

    assert(s.now == 2)
  }

  @Test def snapshot_takesASnapshotWhenTheEventOccurs(): Unit = {
    val e = Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val s = e.snapshot(s1)

    e(1)
    assert(s.now == 2)

    v1.set(2)
    assert(s.now == 2)
    e(1)
    assert(s.now == 3)
  }


  /* delay[T](e: Event[T], init: T, n: Int): Signal[T] */
  @Test def delay_theInitialValueIsSetCorrectly(): Unit = {
    val e = Evt[Int]()
    val s = e.delay(0, 3)

    assert(s.now == 0)
  }

  @Test def delay_takesASnapshotWhenTheEventOccurs(): Unit = {
    val e = Evt[Int]()
    val s = e.delay(0, 3)

    // Initially remains the same for n times
    e(1)
    assert(s.now == 0)
    e(2)
    assert(s.now == 0)
    e(3)
    assert(s.now == 0)

    // Now starts changing
    e(4)
    assert(s.now == 1)
    e(5)
    assert(s.now == 2)
    e(6)
    assert(s.now == 3)
  }

  /* delay[T](signal: Signal[T], n: Int): Signal[T] */
  @Test def delay1_theInitialValueIsSetCorrectly(): Unit = {
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val s = s1.delay(3)

    assert(s.now == 2)
  }

  @Test def delay1_takesASnapshotWhenTheEventOccurs(): Unit = {
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
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
    val e = Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val s2 = e.switchTo(s1)

    assert(s2.now == 2)
    v1.set(2)
    assert(s2.now == 3)
  }

  @Test def switchTo_theEventSwitchesTheValueToTheValueOfTheEvent(): Unit = {
    val e = Evt[Int]()
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
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
    val e = Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map { _ + 1 }
    val s2 = v2.map { _ + 1 }
    val s3 = e.switchOnce(s1, s2)

    assert(s3.now == 1)
    v1.set(1)
    assert(s3.now == 2)
  }

  @Test def switchOnce_theEventSwitchesTheValueToTheValueOfTheOtherSignal(): Unit = {
    val e = Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map { _ + 1 }
    val s2 = v2.map { _ + 1 }
    val s3 = e.switchOnce(s1, s2)

    e(1)
    assert(s3.now == 11)
    e(2)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* reset */
  @Test def reset_TheInitialValueOfTheSignalIsGivenByInitAndTheFactory(): Unit = {
    val e = Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map { _ + 1 }
    val s2 = v2.map { _ + 1 }

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
    val e = Evt[Int]()
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map { _ + 1 }
    val s2 = v2.map { _ + 1 }

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
    val s1 = v1.map { _ + 1 }
    val e = s1.change
    e += { x => test += 1 }

    assert(test == 0)
  }

  @Test def change_isTriggeredWhenTheSignalChanges(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
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
    val s1 = v1.map { _ + 1 }
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
    val s1 = v1.map { _ + 1 }
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    assert(test == 0)
  }

  @Test def changed_isTriggeredWhenTheSignalChanges(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
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
    val s1 = v1.map { _ + 1 }
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
    val s1 = v1.map { _ + 1 }
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => { test += 1 })

    assert(test == 0)
  }

  @Test def changedTo_isTriggeredWhenTheSignalHasTheGivenValue(): Unit = {
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map { _ + 1 }
    val e: Event[Unit] = s1.changedTo(3)
    e += ((x: Unit) => { test += 1 })

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 1)
  }


}
