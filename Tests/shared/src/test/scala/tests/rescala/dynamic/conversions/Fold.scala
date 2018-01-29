package tests.rescala.dynamic.conversions

import tests.rescala.testtools.RETests


class Fold extends RETests {


  /* toggle */
  allEngines("toggle the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val v2 = Var(11)
    val s2 = v2.map {_ + 1}
    val s = e.toggle(s1, s2)

    assert(s.now == 2)
    assert(s2.now == 12)
  }

  allEngines("toggle the Event Switches The Signal") { engine => import engine._
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

  /* switchTo */
  allEngines("switch To the Initial Value Is Set To The Signal") { engine => import engine._
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s2 = e.switchTo(s1)

    assert(s2.now == 2)
    v1.set(2)
    assert(s2.now == 3)
  }

  allEngines("switch To the Event Switches The Value To The Value Of The Event") { engine => import engine._
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
  allEngines("switch Once the Initial Value Is Set To The Signal") { engine => import engine._
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

  allEngines("switch Once the Event Switches The Value To The Value Of The Other Signal") { engine => import engine._
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

}
