package tests.rescala.misc

import tests.rescala.testtools.RETests

class ObserveTests extends RETests {
  multiEngined { engine =>
    import engine._

    test("can observe signals") {
      var result = List[Int]()
      val v1     = Var(0)
      v1.observe(result ::= _)

      assertEquals(result, List(0))

      v1.set(10)

      assertEquals(result, List(10, 0))
    }
//
//  test("self removing observers are possible, although maybe not as straight forward as one would wish?"){
//    var result = List[Int]()
//    val v1 = Var(0)
//    lazy val link: Signal[Int] = Signal.static(v1) { t =>
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
//    assertEquals(result, List(0))
//    v1.set(10)
//    assertEquals(result, List(10, 0))
//    v1.set(20)
//    assertEquals(result, List(10, 0))
//    v1.set(5)
//    assertEquals(result, List(10, 0))
//  }
//
//  test("simpler self removing observers, but this does not fire on attach"){
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
//    assertEquals(result, List())
//    v1.set(10)
//    assertEquals(result, List(10))
//    v1.set(20)
//    assertEquals(result, List(10))
//    v1.set(5)
//    assertEquals(result, List(10))
//  }

  }
}
