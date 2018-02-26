package tests.rescala.dynamic.conversions

import tests.rescala.testtools.RETests


class Fold extends RETests { multiEngined { engine => import engine._


  /* toggle */
  test("toggle the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val v2 = Var(11)
    val s2 = v2.map {_ + 1}
    val s = e.toggle(s1, s2)

    assert(s.readValueOnce == 2)
    assert(s2.readValueOnce == 12)
  }

  test("toggle the Event Switches The Signal") {
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val v2 = Var(11)
    val s2 = v2.map {_ + 1}
    val s = e.toggle(s1, s2)

    assert(s.readValueOnce == 2)
    e.fire(1)
    assert(s.readValueOnce == 12)
    v2.set(12)
    assert(s.readValueOnce == 13)
    v1.set(2)
    assert(s.readValueOnce == 13)
    e.fire(1)
    v1.set(3)
    assert(s.readValueOnce == 4)
    v2.set(13)
    assert(s.readValueOnce == 4)

  }

  /* switchTo */
  test("switch To the Initial Value Is Set To The Signal") {
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s2 = e.switchTo(s1)

    assert(s2.readValueOnce == 2)
    v1.set(2)
    assert(s2.readValueOnce == 3)
  }

  test("switch To the Event Switches The Value To The Value Of The Event") {
    val e = Evt[Int]
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val s2 = e.switchTo(s1)

    e.fire(1)
    assert(s2.readValueOnce == 1)
    e.fire(100)
    assert(s2.readValueOnce == 100)
    v1.set(2)
    assert(s2.readValueOnce == 100)
  }

  /* switchOnce */
  test("switch Once the Initial Value Is Set To The Signal") {
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map {_ + 1}
    val s2 = v2.map {_ + 1}
    val s3 = e.switchOnce(s1, s2)

    assert(s3.readValueOnce == 1)
    v1.set(1)
    assert(s3.readValueOnce == 2)
  }

  test("switch Once the Event Switches The Value To The Value Of The Other Signal") {
    val e = Evt[Int]
    val v1 = Var(0)
    val v2 = Var(10)
    val s1 = v1.map {_ + 1}
    val s2 = v2.map {_ + 1}
    val s3 = e.switchOnce(s1, s2)

    e.fire(1)
    assert(s3.readValueOnce == 11)
    e.fire(2)
    v2.set(11)
    assert(s3.readValueOnce == 12)
  }

} }
