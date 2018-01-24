package tests.rescala.static.signals

import tests.rescala.util.RETests

//These 3 are for JUnitRunner




class VarTestSuite extends RETests {



  allEngines("get Val After Creation Returns Initialization Value"){ engine => import engine._
    val v = Var(1)
    assert(v.now == 1)
  }

  allEngines("get Val Returns Correct Value"){ engine => import engine._
    val v = Var(1)
    v.set(10)
    assert(v.now == 10)
  }


  allEngines("var Notifies Signal Of Changes"){ engine => import engine._
    val v = Var(1)
    val s = v.map { _ + 1 }
    assert(v.now == 1)

    assert(s.now == 2)
    v.set(2)
    assert(v.now == 2)
    assert(s.now == 3)

  }

  allEngines("change Event Only Triggered On Value Change"){ engine => import engine._
    var changes = 0
    val v = Var(1)
    v.observe { _ => changes += 1 }

    assert(changes == 1)
    v.set(2)
    assert(changes == 2)
    v.set(3)
    assert(changes == 3)
    v.set(3)
    assert(changes == 3)
  }

  allEngines("dependant Is Only Invoked On Value Change"){ engine => import engine._
    var changes = 0
    val v = Var(1)
    val s = v.map { i => changes += 1; i + 1 }
    assert(s.now == 2)
    assert(changes == 1)
    v.set(2)
    assert(s.now == 3)
    assert(changes == 2)
    v.set(2)
    assert(changes == 2)
  }

  allEngines("transform Var"){ engine => import engine._
    val v1 = Var(0)
    def inc() = v1.transform(1.+)

    assert(v1.now === 0)
    inc()
    assert(v1.now === 1)

    val s1 = v1.map(identity)

    assert(s1.now === 1)
    inc()
    assert(v1.now === 2)
    assert(s1.now === 2)

  }

}
