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

}
