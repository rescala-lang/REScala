package tests.rescala




class EmptySignalTestSuite extends RETests {

  allEngines("basicEmptySignalTest"){ engine => import engine._

    val v = Var.empty[Int]

    intercept[NoSuchElementException](v.now)

    var res = -100

    v.observe(res = _)

    assert(res == -100, "sanity")

    val s = v.map(1.+)

    intercept[NoSuchElementException](s.now)

    v.set(100)

    assert(res == 100, "observed?")
    assert(v.now == 100, "changed from empty to value")
    assert(s.now == 101, "changed from empty to value 2")



  }

  allEngines("unwrapEmptySignal"){ engine => import engine._
    val v = Var.empty[Event[Int]]
    val e = v.flatten

    var res = -100

    e.observe(res = _)

    assert(res === -100, "sanity")

    val evt = Evt[Int]

    v.set(evt)

    evt.fire(20)

    assert(res === 20, "could unwrap after Val was empty")

  }

}
