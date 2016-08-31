package tests.rescala.conversions

import rescala.graph.Pulse
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import tests.rescala.RETests

import scala.collection.LinearSeq


class IFunTest extends RETests {

  /* fold */
  allEngines("fold_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    assert(s.now == 10)
  }

  allEngines("fold_the Result Signal Increases When Events Occur") { engine => import engine._
    val e = Evt[Int]
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    e.fire(1)
    e.fire(1)
    assert(s.now == 12)
  }


  /* iterate */
  allEngines("iterate_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val f = (x: Int) => x
    val s: Signal[Int] = e.iterate(10)(f)
    assert(s.now == 10)
  }

  allEngines("iterate_the Functionis Executed Every Time The Event Fires") { engine => import engine._
    var test: Int = 0
    val e = Evt[Int]
    val f = (x: Int) => {test += 1; x}
    val s: Signal[Int] = e.iterate(10)(f)
    e.fire(1)
    assert(test == 1)
    e.fire(2)
    assert(test == 2)
    e.fire(1)
    assert(test == 3)
  }

  // TODO: does it make sense ?
  allEngines("iterate the Parameter Is Always The Init Value") { engine => import engine._
    var test: Int = 0
    val e = Evt[Int]
    val f = (x: Int) => {test = x; x + 1}
    val s: Signal[Int] = e.iterate(10)(f)
    e.fire(1)
    assert(test == 10)
    e.fire(2)
    assert(test == 11)
    e.fire(1)
    assert(test == 12)
  }

  allEngines("iterate_the Result Signal IsNever Changed") { engine => import engine._
    var test: Int = 0
    val e = Evt[Int]
    val f = (x: Int) => {test += x; x}
    val s: Signal[Int] = e.iterate(10)(f)
    e.fire(1)
    assert(s.now == 10)
    e.fire(2)
    assert(s.now == 10)
    e.fire(1)
    assert(s.now == 10)
  }

  /* latest */
  allEngines("latest_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.latest(10)

    assert(s.now == 10)
  }

  allEngines("latest_the Functionis Executed Every Time The Event Fires") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.latest(10)

    e.fire(1)
    assert(s.now == 1)
    e.fire(2)
    assert(s.now == 2)
    e.fire(1)
    assert(s.now == 1)
  }


  /* latestOption */
  allEngines("latest Option_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[Option[Int]] = e.latestOption()

    assert(s.now == None)
  }

  allEngines("latest Option_the Functionis Executed Every Time The Event Fires") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[Option[Int]] = e.latestOption()

    e.fire(1)
    assert(s.now == Option(1))
    e.fire(2)
    assert(s.now == Option(2))
    e.fire(1)
    assert(s.now == Option(1))
  }


  /* last */
  allEngines("last_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.now == List())
  }

  allEngines("last_collects The LastN Events") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[LinearSeq[Int]] = e.last(5)


    assert(s.now == LinearSeq())
    e.fire(1)
    assert(s.now == LinearSeq(1))
    e.fire(2)
    assert(s.now == LinearSeq(1, 2))

    e.fire(3)
    e.fire(4)
    e.fire(5)
    assert(s.now == LinearSeq(1, 2, 3, 4, 5))
    e.fire(6)
    assert(s.now == LinearSeq(2, 3, 4, 5, 6))
  }

  /* list */
  allEngines("list_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val s = e.list()

    assert(s.now == List())
  }

  allEngines("list_the Functionis Executed Every Time The Event Fires") { engine => import engine._
    val e = Evt[Int]
    val s = e.list()

    assert(s.now == List())
    e.fire(1)
    assert(s.now == List(1))
    e.fire(2)
    assert(s.now == List(2, 1))

    e.fire(3)
    e.fire(4)
    e.fire(5)
    e.fire(6)
    assert(s.now == List(6, 5, 4, 3, 2, 1))
  }

  /* toggle */
  allEngines("toggle_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val v2 = Var(11)
    val s2 = v2.map {_ + 1}
    val s = e.toggle(s1, s1)

    assert(s.now == 2)
  }

  allEngines("toggle_the Event Switches The Signal") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val v2 = Var(11)
    val s2 = v2.map {_ + 1}
    val s = e.toggle(s1, s2)

    assert(s.now == 2)
    e.fire(1)
    assert(s.now == 12)
    v2.set(12)
    assert(s.now == 13)
    v1.set(2)
    assert(s.now == 13)
    e.fire(1)
    v1.set(3)
    assert(s.now == 4)
    v2.set(13)
    assert(s.now == 4)

  }

  /* snapshot */
  allEngines("snapshot_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s = e.snapshot(s1)

    assert(s.now == 2)
  }

  allEngines("snapshot_takesA Snapshot When The Event Occurs") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s = e.snapshot(s1)

    e.fire(1)
    assert(s.now == 2)

    v1.set(2)
    assert(s.now == 2)
    e.fire(1)
    assert(s.now == 3)
  }


  /* delay[T](e: Event[T], init: T, n: Int): Signal[T] */
  allEngines("delay_the Initial Value IsSet Correctly") { engine => import engine._
    val e = Evt[Int]
    val s = e.delay(0, 3)

    assert(s.now == 0)
  }

  allEngines("delay_takesA Snapshot When The Event Occurs") { engine => import engine._
    val e = Evt[Int]
    val s = e.delay(0, 3)

    // Initially remains the same for n times
    e.fire(1)
    assert(s.now == 0)
    e.fire(2)
    assert(s.now == 0)
    e.fire(3)
    assert(s.now == 0)

    // Now starts changing
    e.fire(4)
    assert(s.now == 1)
    e.fire(5)
    assert(s.now == 2)
    e.fire(6)
    assert(s.now == 3)
  }

  /* delay[T](signal: Signal[T], n: Int): Signal[T] */
  allEngines("delay1_the Initial Value IsSet Correctly") { engine => import engine._
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s = s1.delay(3)

    assert(s.now == 2)
  }

  allEngines("delay1_takesA Snapshot When The Event Occurs") { engine => import engine._
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
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
  allEngines("switch To_the Initial Value IsSet ToThe Signal") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s2 = e.switchTo(s1)

    assert(s2.now == 2)
    v1.set(2)
    assert(s2.now == 3)
  }

  allEngines("switch To_the Event Switches The Value ToThe Value OfThe Event") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s2 = e.switchTo(s1)

    e.fire(1)
    assert(s2.now == 1)
    e.fire(100)
    assert(s2.now == 100)
    v1.set(2)
    assert(s2.now == 100)
  }

  /* switchOnce */
  allEngines("switch Once_the Initial Value IsSet ToThe Signal") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map {_ + 1}
    val s2 = v2.map {_ + 1}
    val s3 = e.switchOnce(s1, s2)

    assert(s3.now == 1)
    v1.set(1)
    assert(s3.now == 2)
  }

  allEngines("switch Once_the Event Switches The Value ToThe Value OfThe Other Signal") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map {_ + 1}
    val s2 = v2.map {_ + 1}
    val s3 = e.switchOnce(s1, s2)

    e.fire(1)
    assert(s3.now == 11)
    e.fire(2)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* reset */
  allEngines("reset_ The Initial Value OfThe Signal IsGiven ByInit And The Factory") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map {_ + 1}
    val s2 = v2.map {_ + 1}

    def factory(x: Int) = x % 2 match {
      case 0 => s1
      case 1 => s2
    }
    val s3 = e.reset(100)(factory)

    assert(s3.now == 1)
    v1.set(1)
    assert(s3.now == 2)

  }

  allEngines("reset_ The Value OfThe Signal IsGiven ByThe Event And The Factory") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map {_ + 1}
    val s2 = v2.map {_ + 1}

    def factory(x: Int) = x % 2 match {
      case 0 => s1
      case 1 => s2
    }

    val s3 = e.reset(100)(factory)

    //assert(s3.get == 1)
    v1.set(1)
    assert(s3.now == 2)
    e.fire(101)
    assert(s3.now == 11)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* change */
  allEngines("change_is Not Triggered OnCreation") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e = s1.change
    e += { x => test += 1 }

    assert(test == 0)
  }

  allEngines("change_is Triggered When The Signal Changes") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e = s1.change
    e += { x => test += 1 }

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  allEngines("change_the Value OfThe Event Reflects The Change InThe Signal") { engine => import engine._
    var test = (0, 0)
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e = s1.change
    e += { x => test = x.pair }

    v1 set 2
    assert(test === ((2, 3)))
    v1 set 3
    assert(test === ((3, 4)))
  }

  /* changed */
  allEngines("changed_is Not Triggered OnCreation") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Int] = s1.changed
    e += ((x: Int) => {test += 1})

    assert(test == 0)
  }

  allEngines("changed_is Triggered When The Signal Changes") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Int] = s1.changed
    e += ((x: Int) => {test += 1})

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  allEngines("changed the Value Of The Event Reflects The Change InThe Signal") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Int] = s1.changed
    e += ((x: Int) => {test = x})

    v1 set 2
    assert(test == 3)
    v1 set 3
    assert(test == 4)
  }

  /* changedTo */
  allEngines("changed To_is Not Triggered On Creation") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => {test += 1})

    assert(test == 0)
  }

  allEngines("changed To is Triggered When The Signal Has The Given Value") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Unit] = s1.changedTo(3)
    e += ((x: Unit) => {test += 1})

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 1)
  }

  allEngines("folding changing and emptyness") { engine => import engine._
    val v1 = Var.empty[String]
    val v2 = Var.empty[String]

    val e1 = v1.changed.map(x => ("constant", x))
    val e2 = v2.change.map(_.pair).recover(_ => "failed" -> "change")

    val ored: Event[(String, String)] = e1 || e2

    val log = ored.log()

    assert(log.now === Nil)

    v1.set("one")
    assert(log.now === List("constant" -> "one"))

    v2.set("two")
    assert(log.now === List("failed" -> "change", "constant" -> "one"))

    v2.set("three")
    assert(log.now === List("two" -> "three", "failed" -> "change", "constant" -> "one"))


    plan(v1, v2) { turn =>
      v1.admit("four a")(turn)
      v2.admit("four b")(turn)
    }

    assert(log.now === List("constant" -> "four a", "two" -> "three", "failed" -> "change", "constant" -> "one"))



    plan(v1, v2) { turn =>
      v1.admitPulse(Pulse.Exceptional(EmptySignalControlThrowable))(turn)
      v2.admit("five b")(turn)
    }

    assert(log.now === List("four b" -> "five b", "constant" -> "four a", "two" -> "three", "failed" -> "change", "constant" -> "one"))

  }


}
