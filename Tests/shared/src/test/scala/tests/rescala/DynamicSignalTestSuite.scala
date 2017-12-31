package tests.rescala


import org.scalatest.prop.Whenever
import rescala.Engines
import rescala.Infiltrator.assertLevel




class DynamicSignalTestSuite extends RETests with Whenever {



  allEngines("signal Reevaluates The Expression When Something It Depends On Is Updated"){ engine => import engine._
    val v = Var(0)
    var i = 1
    val s = Signal { v() + i }
    i = 2
    v.set(2)
    assert(s.now == 4)
  }

  allEngines("the Expression Is Note Evaluated Every Time now Is Called"){ engine => import engine._
    var a = 10
    val s = Signal(1 + 1 + a)
    assert(s.now === 12)
    a = 11
    assert(s.now === 12)
  }


  allEngines("simple Signal Returns Correct Expressions"){ engine => import engine._
    val s = Signal(1 + 1 + 1)
    assert(s.now === 3)
  }

  allEngines("the Expression Is Evaluated Only Once"){ engine => import engine._

    var a = 0
    val v = Var(10)
    val s1 = Signal {a += 1 : @unchecked; v() % 10 }
    val s2 = Signal { a }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
    assert(s1.now == 1)
    assert(s2.now == 1)
  }

  allEngines("handlers Are Executed"){ engine => import engine._

    var test = 0
    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test == 0)

    v.set(3)
    assert(test == 3)

  }

  allEngines("level Is Correctly Computed"){ engine => import engine._

    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    assertLevel(v, 0)
    assertLevel(s1, 1)
    assertLevel(s2, 1)
    assertLevel(s3, 2)


  }


  /* Specific of SignalSynt */


  allEngines("signal Does Not Reevaluate The Expression If Depends On IsUpdated That Is Not In Current Dependencies"){ engine => import engine._
    val condition = Var(true)
    val ifTrue = Var(0)
    val ifFalse = Var(10)
    var reevaluations = 0
    val s = Signals.dynamic(condition, ifTrue, ifFalse) { (dt: DynamicTicket) =>
      reevaluations += 1
      if (dt.depend(condition)) dt.depend(ifTrue) else dt.depend(ifFalse)
    }

    assert(reevaluations == 1)
    assert(s.now == 0)
    ifTrue.set(1)
    assert(reevaluations == 2)
    assert(s.now == 1)
    ifFalse.set(11) // No effect
    assert(reevaluations == 2)
    assert(s.now == 1)

    condition.set(false)
    assert(reevaluations == 3)
    assert(s.now == 11)
    ifFalse.set(12)
    assert(reevaluations == 4)
    assert(s.now == 12)
    ifTrue.set(2) // No effect
    assert(reevaluations == 4)
    assert(s.now == 12)
  }


  allEngines("keep fixed Dependencies"){ engine => import engine._

    val v1 = Var(true)
    val v2 = Var(0)
    val v3 = Var(10)
    var i = 0
    var test = 0

    val s = Signal  {
      i += 1
      if (v1()) v2() else v3()
    }

    val e = s.change
    e += (x => test += 1)

    assert(test == 0)
    v2.set(1)
    assert(test == 1)

    v1.set(false)
    assert(test == 2)
    v3.set(11)
    assert(test == 3)

    v2.set(2)
    assert(test == 3)

    v1.set(true)
    assert(test == 4)
    v2.set(3)
    assert(test == 5)


  }

  allEngines("dependant Is Only Invoked On Value Changes"){ engine => import engine._
    var changes = 0
    val v = Var(1)
    val s = Signal {
      changes += 1; v() + 1
    }
    assert(changes === 1)
    assert(s.now === 2)
    v.set(2)
    assert(s.now === 3)
    assert(changes === 2)
    v.set(2)
    assert(changes === 2) // is actually 3
  }

  allEngines("creating Signals Inside Signals") { engine => import engine._

    // ignore for locksweep, as it does not support predeclared levels, so would run into an endless loop below
    whenever(engine != Engines.locksweep &&
      engine != rescala.Engines.parallellocksweep) {

      val outside = Var(1)

      val testsig = dynamic() { t =>
        //remark 01.10.2014: without the bound the inner signal will be enqueued (it is level 0 same as its dependency)
        //this will cause testsig to reevaluate again, after the inner signal is fully updated.
        // leading to an infinite loop
        t.depend(dynamic(outside) { t => t.depend(outside) })
      }

      assert(testsig.now === 1)
      outside set 2
      assert(testsig.now === 2)
    }
  }

  allEngines("creating Signals Inside Signals Workaround"){ engine => import engine._


    val outside = Var(1)

    val dynsig: Signal[Signal[Int]] = Signal { Signal { outside() } }
    val testsig = dynsig.flatten

      assert(testsig.now === 1)
    outside set 2
    assert(testsig.now === 2)
  }

  allEngines("dynamic dependency changes ontop of stuff that is not changing"){ engine => import engine._
    val v0 = Var("level 0")
    val v3 = v0.map(_ => "level 1").map(_ => "level 2").map(_ => "level 3")

    val condition = Var(false)
    val `dynamic signal changing from level 1 to level 4` = dynamic(condition) { t =>
      if (t.depend(condition)) t.depend(v3) else t.depend(v0)
    }
    assert(`dynamic signal changing from level 1 to level 4`.now == "level 0")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 1)

    condition.set(true)
    assert(`dynamic signal changing from level 1 to level 4`.now == "level 3")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 4)
  }

  allEngines("creating signals in signals based on changing signals"){ engine => import engine._
    val v0 = Var("level 0")
    val v3 = v0.map(_ + "level 1").map(_  + "level 2").map(_ + "level 3")

    val `dynamic signal changing from level 1 to level 5` = Signal {
      if (v0() == "level 0") v0() else {
        v3.map(_ + "level 4 inner").apply()
      }
    }
    assert(`dynamic signal changing from level 1 to level 5`.now == "level 0")
    //note: will start with level 5 because of static guess of current level done by the macro expansion
    assertLevel(`dynamic signal changing from level 1 to level 5`, 5)

    v0.set("level0+")
    assert(`dynamic signal changing from level 1 to level 5`.now == "level0+level 1level 2level 3level 4 inner")
    assertLevel(`dynamic signal changing from level 1 to level 5`, 5)
  }

  allEngines("creating signals in signals based on changing signals dynamic"){ engine => import engine._
    val v0 = Var("level 0")
    val v3 = v0.map(_ + "level 1").map(_  + "level 2").map(_ + "level 3")

    val `dynamic signal changing from level 1 to level 4` = dynamic() { implicit ticket =>
      if (ticket.depend(v0) == "level 0") ticket.depend(v0) else {
        // the static bound is necessary here, otherwise we get infinite loops
        ticket.depend(dynamic(v3) {t => t.depend(v3) + "level 4 inner" })
      }
    }
    assert(`dynamic signal changing from level 1 to level 4`.now == "level 0")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 1)

    v0.set("level0+")
    assert(`dynamic signal changing from level 1 to level 4`.now == "level0+level 1level 2level 3level 4 inner")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 5)
  }
}
