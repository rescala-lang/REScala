package tests.rescala.events

import tests.rescala.RETests

class MacroEventTestSuite extends RETests {


  allEngines("simple"){ engine => import engine._
    val ev1 = Evt[Int]
    val v1 = Var(8)
    val snapshotEvent = Event {
      ev1().map(i => i + v1())
    }

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1.fire(10)
    assert(res.now === 18)
    v1.set(7)
    assert(res.now === 18)
    ev1.fire(10)
    assert(res.now === 17)

  }

  allEngines("map"){ engine => import engine._
    val ev1 = Evt[Int]
    val v1 = Var(8)
    val snapshotEvent = ev1.map(i =>  i + v1() )

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1.fire(10)
    assert(res.now === 18)
    v1.set(7)
    assert(res.now === 18)
    ev1.fire(10)
    assert(res.now === 17)

  }

  allEngines("map as static"){ engine => import engine._
    val ev1 = Evt[Int]
    val snapshotEvent = ev1.map(i =>  i + 1 )

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1.fire(10)
    assert(res.now === 11)
    ev1.fire(20)
    assert(res.now === 21)
    ev1.fire(10)
    assert(res.now === 11)

  }


  allEngines("use Events In Signal Expression") { engine => import engine._
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val res = Signal {List(e1(), e2()).flatten.sum}

    assert(res.now === 0)
    e1.fire(10)
    assert(res.now === 10)
    e2.fire(11)
    assert(res.now === 11)
    update(e1 -> 10, e2 -> 10)
    assert(res.now === 20)

  }

  allEngines("use Event Expression") { engine => import engine._
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val event = Event {Some(List(e1(), e2()).flatten)}
    val res = event.latest(Nil)

    assert(res.now === Nil)
    e1.fire(9)
    assert(res.now === List(9))
    e2.fire(10)
    assert(res.now === List(10))
    update(e1 -> 11, e2 -> 12)
    assert(res.now === List(11, 12))
  }


  allEngines("higher order events") { engine => import engine._
    val e1 = Evt[Signal[Int]]

    val event =  Event { Some{ e1.value.get.value } }

    val res = event.latest()

    e1.fire(Signal(1))
    assert(res.now === 1)
    e1.fire(Signal(2))
    assert(res.now === 2)
    e1.fire(Signal(3))
    assert(res.now === 3)

  }


  allEngines("cut out created signals") { engine => import engine._
    val e1 = Evt[Int]

    val event =  Event { Some{ e1.count.value } }

    val res = event.latest()

    e1.fire(1)
    assert(res.now === 1)
    e1.fire(2)
    assert(res.now === 2)
    e1.fire(3)
    assert(res.now === 3)

  }

}
