package tests.rescala

import tests.rescala.util.RETests

class ObserveTests extends RETests {

  allEngines("can observe signals") { engine => import engine._
    var result = List[Int]()
    val v1 = Var(0)
    v1.observe(result ::= _)

    assert(result === List(0))

    v1.set(10)

    assert(result === List(10, 0))
  }
//
//  allEngines("self removing observers are possible, although maybe not as straight forward as one would wish?"){ engine => import engine._
//    var result = List[Int]()
//    val v1 = Var(0)
//    lazy val link: Signal[Int] = Signals.static(v1) { t =>
//      val v = v1.get(t)
//      if (v > 10) {
//        t.drop(link)(v1)
//        obs.remove()
//      }
//      v
//    }
//
//    lazy val obs = link.observe(result ::= _)
//
//    // we need this to force the observer into existence
//    obs
//
//    assert(result === List(0))
//    v1.set(10)
//    assert(result === List(10, 0))
//    v1.set(20)
//    assert(result === List(10, 0))
//    v1.set(5)
//    assert(result === List(10, 0))
//  }
//
//  allEngines("simpler self removing observers, but this does not fire on attach"){ engine => import engine._
//    var result = List[Int]()
//    val v1 = Var(0)
//
//    lazy val obs: Event[Int] = Events.static("obs", v1) { t =>
//      val v = v1.get(t)
//      if (v > 10) t.drop(obs)(v1)
//      else t.schedule(once[Int](obs, Some(v), result ::= _))
//      v1.pulse(t)
//    }
//
//    // we need this to force the observer into existence
//    obs
//
//    assert(result === List())
//    v1.set(10)
//    assert(result === List(10))
//    v1.set(20)
//    assert(result === List(10))
//    v1.set(5)
//    assert(result === List(10))
//  }

}
