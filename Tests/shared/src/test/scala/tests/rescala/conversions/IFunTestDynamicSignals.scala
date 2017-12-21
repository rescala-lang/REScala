package tests.rescala.conversions

import tests.rescala.RETests


class IFunTestDynamicSignals extends RETests {



  /* count */
  allEngines("count the Initial Value Is Set Correctly"){ engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.count
    assert(s.now == 0)
  }

  allEngines("count the Result Signal Increases When Events Occur"){ engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.count
    e.fire(1)
    e.fire(1)
    assert(s.now == 2)
  }

  /* toggle */
  allEngines("toggle the Initial Value Is Set Correctly"){ engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val v2 = Var(11)
    val s2 = v2.map(_ + 1)
    val s = e.toggle(s1, s2)

    assert(s.now == 2)
  }

  allEngines("toggle the Event Switches The Signal"){ engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val v2 = Var(11)
    val s2 = v2.map(_ + 1)
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

  /* delay[T](signal: Signal[T], n: Int): Signal[T] */
//  allEngines("delay1 the Initial Value Is Set Correctly"){ engine => import engine._
//    val v1 = Var(1)
//    val s1 = v1.map(_ + 1)
//    val s = s1.delay(3)
//
//    assert(s.now == 2)
//  }
//
//  allEngines("delay1 takesA Snapshot When The Event Occurs"){ engine => import engine._
//    val v1 = Var(1)
//    val s1 = v1.map(_ + 1)
//    val s = s1.delay(3)
//
//    // Initially remains the same for n times
//    v1.set(2)
//    assert(s.now == 2)
//    v1.set(3)
//    assert(s.now == 2)
//    v1.set(4)
//    assert(s.now == 2)
//
//    // Now starts changing
//    v1.set(5)
//    assert(s.now == 3)
//    v1.set(6)
//    assert(s.now == 4)
//  }

  /* switchTo */
  allEngines("switch To the Initial Value Is Set To The Signal"){ engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s2 = e.switchTo(s1)

    assert(s2.now == 2)
    v1.set(2)
    assert(s2.now == 3)
  }

  allEngines("switch To the Event Switches The Value To The Value Of The Event"){ engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val s2 = e.switchTo(s1)

    e.fire(1)
    assert(s2.now == 1)
    e.fire(100)
    assert(s2.now == 100)
    v1.set(2)
    assert(s2.now == 100)
  }

  /* switchOnce */
  allEngines("switch Once the Initial Value Is Set To The Signal"){ engine => import engine._
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map(_ + 1)
    val s2 = v2.map(_ + 1)
    val s3 = e.switchOnce(s1, s2)

    assert(s3.now == 1)
    v1.set(1)
    assert(s3.now == 2)
  }

  allEngines("switch Once the Event Switches The Value To The Value Of The Other Signal"){ engine => import engine._
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map(_ + 1)
    val s2 = v2.map(_ + 1)
    val s3 = e.switchOnce(s1, s2)

    e.fire(1)
    assert(s3.now == 11)
    e.fire(2)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* reset */
  allEngines("reset  The Initial Value Of The Signal Is Given By Init And The Factory"){ engine => import engine._
    val e = Evt[Int]
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

  allEngines("reset  The Value Of The Signal Is Given By The Event And The Factory"){ engine => import engine._
    val e = Evt[Int]
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
    e.fire(101)
    assert(s3.now == 11)
    v2.set(11)
    assert(s3.now == 12)
  }

  /* changed */
  allEngines("changed is Not Triggered On Creation"){ engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e: Event[Int] = s1.changed
    e += ((x: Int) => { test += 1 })

    assert(test == 0)
  }

  allEngines("changed is Triggered When The Signal Changes"){ engine => import engine._
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

  allEngines("changed the Value Of The Event Reflects The Change In The Signal"){ engine => import engine._
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
  allEngines("changed To is Not Triggered On Creation"){ engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map(_ + 1)
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => { test += 1 })

    assert(test == 0)
  }

  allEngines("changed To is Triggered When The Signal Has The Given Value"){ engine => import engine._
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
