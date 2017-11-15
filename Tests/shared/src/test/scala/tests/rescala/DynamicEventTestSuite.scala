package tests.rescala




class DynamicEventTestSuite extends RETests {

  allEngines("simple"){ engine => import engine._
    val ev1 = Evt[Int]
    val v1 = Var(8)
    val snapshotEvent = Event {
      ev1().map(i => i + v1())
    }

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1(10)
    assert(res.now === 18)
    v1.set(7)
    assert(res.now === 18)
    ev1(10)
    assert(res.now === 17)

  }

  allEngines("map"){ engine => import engine._
    val ev1 = Evt[Int]
    val v1 = Var(8)
    val snapshotEvent = ev1.map(i =>  i + v1() )

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1(10)
    assert(res.now === 18)
    v1.set(7)
    assert(res.now === 18)
    ev1(10)
    assert(res.now === 17)

  }

  allEngines("map as static"){ engine => import engine._
    val ev1 = Evt[Int]
    val snapshotEvent = ev1.map(i =>  i + 1 )

    val res = snapshotEvent.latest(0)

    assert(res.now === 0)
    ev1(10)
    assert(res.now === 11)
    ev1(20)
    assert(res.now === 21)
    ev1(10)
    assert(res.now === 11)

  }

}
