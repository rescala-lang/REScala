package tests.rescala

//These 3 are for JUnitRunner

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn




class VarTestSuite extends RETests {



  allEngines("getValAfterCreationReturnsInitializationValue"){ engine => import engine._
    val v = Var(1)
    assert(v.now == 1)
  }

  allEngines("getValReturnsCorrectValue"){ engine => import engine._
    val v = Var(1)
    v.set(10)
    assert(v.now == 10)
  }


  allEngines("varNotifiesSignalOfChanges"){ engine => import engine._
    val v = Var(1)
    val s = v.map { _ + 1 }
    assert(v.now == 1)

    assert(s.now == 2)
    v.set(2)
    assert(v.now == 2)
    assert(s.now == 3)

  }

  allEngines("changeEventOnlyTriggeredOnValueChange"){ engine => import engine._
    var changes = 0
    val v = Var(1)
    val changed = v.change
    changed += { _ => changes += 1 }

    v.set(2)
    assert(changes == 1)
    v.set(3)
    assert(changes == 2)
    v.set(3)
    assert(changes == 2)
  }

  allEngines("dependantIsOnlyInvokedOnValueChange"){ engine => import engine._
    var changes = 0
    val v = Var(1)
    val s = v.map { i => changes += 1; i + 1 }
    assert(s.now == 2)
    v.set(2)
    assert(changes == 2)
    v.set(2)
    assert(changes == 2)
  }

  allEngines("transformVar"){ engine => import engine._
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
