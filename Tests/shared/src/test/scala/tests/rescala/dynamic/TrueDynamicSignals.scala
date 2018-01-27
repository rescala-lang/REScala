package tests.rescala.dynamic

import rescala.Engines
import rescala.core.infiltration.Infiltrator.assertLevel
import tests.rescala.util.RETests

class TrueDynamicSignals extends RETests {

  allEngines("signals Nested In Vars"){ engine => import engine._

    val a = Var(3)
    val b = Var(Signal(a()))
    val c = Signal.dynamic(b()())

    assert(c.now === 3)
    a set 4
    assert(c.now === 4)
    b set Signal(5)
    assert(c.now === 5)

  }

  allEngines("nested Defined Signals"){ engine => import engine._
    val a = Var(3)
    val b = Signal.dynamic {
      val c = Signal { a() }
      c()
    }

    assert(b.now === 3)
    a set 4
    assert(b.now === 4)
    a set 5
    assert(b.now === 5)
  }

  allEngines("use Of Inside Signal"){ engine => import engine._
    val outside = Var(1)
    val inside = Var(10)

    def sig = Signal { outside() }

    val testsig = Signal.dynamic {
      def sig = Signal { inside() }
      sig()
    }

    assert(testsig.now === 10)
    outside set 2
    inside set 11
    assert(testsig.now === 11)
    assert(sig.now === 2)
  }

  allEngines("use Of Outside Signal"){ engine => import engine._
    val outside = Var(1)
    val inside = Var(10)

    def sig()(implicit turnSource: CreationTicket) = Signal { outside() }

    val testsig = Signal.dynamic {
      {
        def sig = Signal { inside() }
        sig()
      }
      sig().apply()
    }

    assert(testsig.now === 1)
    outside set 2
    inside set 11
    assert(testsig.now === 2)
  }

  allEngines("pattern Matching Anonymous Function Nested Signals"){ engine => import engine._
    val v1 = Var(1)
    val v2 = Var(2)
    val s1 = Signal { List(Some(v1), None, Some(v2), None) }
    val s2 = Signal.dynamic {
      s1() collect { case Some(n) => n() }
    }
    assert(s2.now === List(1, 2))
    v1.set(10)
    assert(s2.now === List(10, 2))
  }

  allEngines("outer And Inner Values"){ engine => import engine._
    val v = Var(0)
    object obj {
      def sig = Signal { v() }
    }

    val evt = Evt[Int]

    val testsig = Signal.dynamic {
      val localsig = obj.sig
      val latest = evt latest -1

      localsig() + latest()
    }

    assert(testsig.now === -1)
    evt.fire(100)
    assert(testsig.now === 100)
    v set 10
    assert(testsig.now === 110)
    evt.fire(10)
    assert(testsig.now === 20)
    evt.fire(5)
    assert(testsig.now === 15)
    v set 50
    assert(testsig.now === 55)
  }

  allEngines("chained Signals2"){ engine => import engine._

    import scala.language.reflectiveCalls

    val v1 = Var { 20 }
    val v2 = Var { new {def signal = Signal { v1() } } }
    val v3 = Var { new {val signal = Signal { v2() } } }

    val s = Signal { v3() }

    val sig = Signal.dynamic { s().signal().signal(): @unchecked }

    assert(sig.now === 20)
    v1 set 30
    assert(sig.now === 30)
    v2 set new {def signal = Signal { 7 + v1() } }
    assert(sig.now === 37)
    v1 set 10
    assert(sig.now === 17)
    v3 set new {val signal = Signal { new {def signal = Signal { v1() } } } }
    assert(sig.now === 10)
    v2 set new {def signal = Signal { 10 + v1() } }
    assert(sig.now === 10)
    v1 set 80
    assert(sig.now === 80)
  }

  allEngines("extracting Signal Side Effects"){ engine => import engine._
    val e1 = Evt[Int]
    def newSignal(): Signal[Int] = e1.count()

    val macroRes = Signal {
      newSignal()()
    }
    val normalRes = Signals.dynamic() { t: DynamicTicket =>
      t.read(newSignal())
    }
    assert(macroRes.now === 0, "before, macro")
    assert(normalRes.now === 0, "before, normal")
    e1.fire(1)
    assert(macroRes.now === 1, "after, macro")
    assert(normalRes.now === 1, "after, normal")
    e1.fire(1)
    assert(macroRes.now === 2, "end, macro")
    assert(normalRes.now === 1, "end, normal")
  }

  allEngines("chained Signals1"){ engine => import engine._

    import scala.language.reflectiveCalls

    val v1 = Var { 1 }
    val v2 = Var { 2 }
    val v = Var { List(new {val s = v1 }, new {val s = v2 }) }

    val sig = Signal.dynamic { v() map (_.s()) }

    assert(sig.now === List(1, 2))
    v1 set 5
    assert(sig.now === List(5, 2))
    v2 set 7
    assert(sig.now === List(5, 7))
    v set v.now.reverse
    assert(sig.now === List(7, 5))
  }


  allEngines("signal Does Not Reevaluate The Expression If Depends On IsUpdated That Is Not In Current Dependencies"){ engine => import engine._
    val condition = Var(true)
    val ifTrue = Var(0)
    val ifFalse = Var(10)
    var reevaluations = 0
    val s = Signals.dynamic(condition, ifTrue, ifFalse) { (dt: DynamicTicket) =>
      reevaluations += 1
      if (dt.read(condition)) dt.read(ifTrue) else dt.read(ifFalse)
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











  allEngines("basic Higher Order Signal can Be Accessed"){ engine => import engine._
    val v = Var(42)
    val s1: Signal[Int] = v.map(identity)
    val s2: Signal[Signal[Int]] = dynamic() { t => s1 }

    assert(s2.now.now == 42)

    v.set(0)
    assert(s2.now.now == 0)
  }




  allEngines("creating Signals Inside Signals") { engine => import engine._

    // ignore for locksweep, as it does not support predeclared levels, so would run into an endless loop below
    whenever(engine != Engines.locksweep) {

      val outside = Var(1)

      val testsig = dynamic() { t =>
        //remark 01.10.2014: without the bound the inner signal will be enqueued (it is level 0 same as its dependency)
        //this will cause testsig to reevaluate again, after the inner signal is fully updated.
        // leading to an infinite loop
        t.read(dynamic(outside) { t => t.read(outside) })
      }

      assert(testsig.now === 1)
      outside set 2
      assert(testsig.now === 2)
    }
  }


  allEngines("dynamic dependency changes ontop of stuff that is not changing"){ engine => import engine._
    val v0 = Var("level 0")
    val v3 = v0.map(_ => "level 1").map(_ => "level 2").map(_ => "level 3")

    val condition = Var(false)
    val `dynamic signal changing from level 1 to level 4` = dynamic(condition) { t =>
      if (t.read(condition)) t.read(v3) else t.read(v0)
    }
    assert(`dynamic signal changing from level 1 to level 4`.now == "level 0")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 1)

    condition.set(true)
    assert(`dynamic signal changing from level 1 to level 4`.now == "level 3")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 4)
  }

  allEngines("creating signals in signals based on changing signals dynamic"){ engine => import engine._
    val v0 = Var("level 0")
    val v3 = v0.map(_ + "level 1").map(_  + "level 2").map(_ + "level 3")

    val `dynamic signal changing from level 1 to level 4` = dynamic() { implicit ticket =>
      if (ticket.read(v0) == "level 0") ticket.read(v0) else {
        // the static bound is necessary here, otherwise we get infinite loops
        ticket.read(dynamic(v3) {t => t.read(v3) + "level 4 inner" })
      }
    }
    assert(`dynamic signal changing from level 1 to level 4`.now == "level 0")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 1)

    v0.set("level0+")
    assert(`dynamic signal changing from level 1 to level 4`.now == "level0+level 1level 2level 3level 4 inner")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 5)
  }

}
