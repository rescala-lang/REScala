package tests.rescala


class MacroEventTestSuite extends RETests {


  allEngines("use Events In Signal Expression") { engine => import engine._
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val res = Signal {List(e1(), e2()).flatten.sum}

    assert(res.now === 0)
    e1(10)
    assert(res.now === 10)
    e2(11)
    assert(res.now === 11)
    engine.transaction(e1, e2) { t =>
      e1.admit(10)(t)
      e2.admit(10)(t)
    }
    assert(res.now === 20)

  }

  allEngines("use Event Expression") { engine => import engine._
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val event = Event {Some(List(e1(), e2()).flatten)}
    val res = event.latest(Nil)

    assert(res.now === Nil)
    e1(9)
    assert(res.now === List(9))
    e2(10)
    assert(res.now === List(10))
    engine.transaction(e1, e2) { t =>
      e1.admit(11)(t)
      e2.admit(12)(t)
    }
    assert(res.now === List(11, 12))

  }

}
