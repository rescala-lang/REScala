package tests.rescala.dynamic

import reactives.core.infiltration.Infiltrator
import reactives.core.{CreationTicket, DynamicTicket}
import reactives.default.global.State as BundleState
import reactives.operator.Interface
import reactives.scheduler.Levelbased
import tests.rescala.testtools.RETests

class TrueDynamicSignals extends RETests {
  val ie = new Infiltrator(reactives.default)
  import ie.api.*
  import ie.assertLevel

  test("signals Nested In Vars") {

    val a = Var(3)
    val b = Var(Signal(a.value))
    val c = Signal.dynamic(b.value.value)

    assertEquals(c.readValueOnce, 3)
    a set 4
    assertEquals(c.readValueOnce, 4)
    b set Signal(5)
    assertEquals(c.readValueOnce, 5)

  }

  test("nested Defined Signals") {
    val a = Var(3)
    val b = Signal.dynamic {
      val c = Signal { a.value }
      c.value
    }

    assertEquals(b.readValueOnce, 3)
    a set 4
    assertEquals(b.readValueOnce, 4)
    a set 5
    assertEquals(b.readValueOnce, 5)
  }

  test("use Of Inside Signal") {
    val outside = Var(1)
    val inside  = Var(10)

    def sig = Signal { outside.value }

    val testsig = Signal.dynamic {
      def sig = Signal { inside.value }
      sig.value
    }

    assertEquals(testsig.readValueOnce, 10)
    outside set 2
    inside set 11
    assertEquals(testsig.readValueOnce, 11)
    assertEquals(sig.readValueOnce, 2)
  }

  test("use Of Outside Signal") {
    val outside = Var(1)
    val inside  = Var(10)

    def sig()(implicit turnSource: CreationTicket[global.State]) = Signal { outside.value }

    val testsig = Signal.dynamic {
      {
        def insideSig = Signal { inside.value }
        insideSig.value
      }
      sig().value
    }

    assertEquals(testsig.readValueOnce, 1)
    outside set 2
    inside set 11
    assertEquals(testsig.readValueOnce, 2)
  }

  test("outer And Inner Values") {
    val v = Var(0)
    object obj {
      def sig(implicit ct: CreationTicket[BundleState]) = Signal { v.value }
    }

    val evt = Evt[Int]()
    val testsig = Signal.dynamic {
      obj.sig.value + (evt `hold` -1).value
    }

    assertEquals(testsig.readValueOnce, -1)
    evt.fire(100)
    assertEquals(testsig.readValueOnce, 100)
    v set 10
    assertEquals(testsig.readValueOnce, 110)
    evt.fire(10)
    assertEquals(testsig.readValueOnce, 20)
    evt.fire(5)
    assertEquals(testsig.readValueOnce, 15)
    v set 50
    assertEquals(testsig.readValueOnce, 55)
  }

  // test("chained Signals2") {
  //
  //  import scala.language.reflectiveCalls
  //
  //  val v1 = Var { 20 }
  //  val v2 = Var { new { def signal(implicit ct: CreationTicket[REState]) = Signal { v1() } } }
  //  val v3 = Var { new { val signal = Signal { v2() } } }
  //
  //  val s = Signal { v3() }
  //
  //  val sig = Signal.dynamic { s().signal().signal.value }
  //
  //  assertEquals(sig.readValueOnce, 20)
  //  v1 set 30
  //  assertEquals(sig.readValueOnce, 30)
  //  v2 set new { def signal(implicit ct: CreationTicket[REState]) = Signal { 7 + v1() } }
  //  assertEquals(sig.readValueOnce, 37)
  //  v1 set 10
  //  assertEquals(sig.readValueOnce, 17)
  //  v3 set new { val signal = Signal { new { def signal(implicit ct: CreationTicket[REState]) = Signal { v1() } } } }
  //  assertEquals(sig.readValueOnce, 10)
  //  v2 set new { def signal(implicit ct: CreationTicket[REState]) = Signal { 10 + v1() } }
  //  assertEquals(sig.readValueOnce, 10)
  //  v1 set 80
  //  assertEquals(sig.readValueOnce, 80)
  // }

  test("extracting Signal Side Effects") {
    val e1                                                                 = Evt[Int]()
    def newSignal()(implicit ct: CreationTicket[BundleState]): Signal[Int] = e1.count()

    val macroRes = Signal {
      newSignal().value
    }
    val normalRes = Signal.dynamic() { implicit t: DynamicTicket[BundleState] =>
      t.depend(newSignal())
    }
    assertEquals(macroRes.readValueOnce, 0, "before, macro")
    assertEquals(normalRes.readValueOnce, 0, "before, normal")
    e1.fire(1)
    assertEquals(macroRes.readValueOnce, 1, "after, macro")
    assertEquals(normalRes.readValueOnce, 1, "after, normal")
    e1.fire(1)
    assertEquals(macroRes.readValueOnce, 2, "end, macro")
    assertEquals(normalRes.readValueOnce, 1, "end, normal")
  }

  // test("chained Signals1") {
  //
  //  import scala.language.reflectiveCalls
  //
  //  val v1 = Var { 1 }
  //  val v2 = Var { 2 }
  //  val v  = Var { List(new { val s = v1 }, new { val s = v2 }) }
  //
  //  val sig = Signal.dynamic { v() map (_.s()) }
  //
  //  assertEquals(sig.readValueOnce, List(1, 2))
  //  v1 set 5
  //  assertEquals(sig.readValueOnce, List(5, 2))
  //  v2 set 7
  //  assertEquals(sig.readValueOnce, List(5, 7))
  //  v set v.readValueOnce.reverse
  //  assertEquals(sig.readValueOnce, List(7, 5))
  // }

  test("signal Does Not Reevaluate The Expression If Depends On IsUpdated That Is Not In Current Dependencies") {
    val condition     = Var(true)
    val ifTrue        = Var(0)
    val ifFalse       = Var(10)
    var reevaluations = 0
    val s = Signal.dynamic(condition) { (dt: DynamicTicket[BundleState]) =>
      reevaluations += 1
      if (dt.depend(condition)) dt.depend(ifTrue) else dt.depend(ifFalse)
    }

    assert(reevaluations == 1)
    assert(s.readValueOnce == 0)
    ifTrue.set(1)
    assert(reevaluations == 2)
    assert(s.readValueOnce == 1)
    ifFalse.set(11) // No effect
    assert(reevaluations == 2)
    assert(s.readValueOnce == 1)

    condition.set(false)
    assert(s.readValueOnce == 11)
    assert(reevaluations == 3)
    ifFalse.set(12)
    assert(s.readValueOnce == 12)
    assert(reevaluations == 4)
    ifTrue.set(2) // No effect
    assert(s.readValueOnce == 12)
    assert(reevaluations == 4)
  }

  test("basic Higher Order Signal can Be Accessed") {
    val v                       = Var(42)
    val s1: Signal[Int]         = v.map(identity)
    val s2: Signal[Signal[Int]] = Signal.dynamic() { _ => s1 }

    assert(s2.readValueOnce.readValueOnce == 42)

    v.set(0)
    assert(s2.readValueOnce.readValueOnce == 0)
  }

  test("creating Signals Inside Signals") {
    val outside = Var(1)

    val testsig = Signal.dynamic() { implicit to =>
      // remark 01.10.2014: without the bound the inner signal will be enqueued (it is level 0 same as its dependency)
      // this will cause testsig to reevaluate again, after the inner signal is fully updated.
      // leading to an infinite loop
      to.depend(Signal.dynamic(outside) { ti => ti.depend(outside) })
    }

    assertEquals(testsig.readValueOnce, 1)
    outside set 2
    assertEquals(testsig.readValueOnce, 2)
  }

  test("dynamic dependency changes on top of stuff that is not changing") {
    val v0 = Var("level 0")
    val v3 = v0.map(_ => "level 1").map(_ => "level 2").map(_ => "level 3")

    val condition = Var(false)
    val `dynamic signal changing from level 1 to level 4` = Signal.dynamic(condition) { t =>
      if (t.depend(condition)) t.depend(v3) else t.depend(v0)
    }
    assert(`dynamic signal changing from level 1 to level 4`.readValueOnce == "level 0")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 1)

    condition.set(true)
    assert(`dynamic signal changing from level 1 to level 4`.readValueOnce == "level 3")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 4)
  }

  test("creating signals in signals based on changing signals dynamic") {
    val v0 = Var("level 0")
    val v3 = v0.map(_ + "level 1").map(_ + "level 2").map(_ + "level 3")

    val `dynamic signal changing from level 1 to level 4` = Signal.dynamic() { implicit ticket =>
      if (ticket.depend(v0) == "level 0") ticket.depend(v0)
      else {
        // the static bound is necessary here, otherwise we get infinite loops
        ticket.depend(Signal.dynamic(v3) { t => t.depend(v3) + "level 4 inner" })
      }
    }
    assert(`dynamic signal changing from level 1 to level 4`.readValueOnce == "level 0")
    assertLevel(`dynamic signal changing from level 1 to level 4`, 1)

    v0.set("level0+")
    assert(
      `dynamic signal changing from level 1 to level 4`.readValueOnce == "level0+level 1level 2level 3level 4 inner"
    )
    assertLevel(`dynamic signal changing from level 1 to level 4`, 5)
  }

  test("graph cost example") {

    def mini(x: Var[Map[Signal[Int], Int]]): Signal[Int] =
      Signal.dynamic {
        val (node, value) = x.value.minBy { case (n, v) => n.value + v }
        node.value + value
      }

    val root = Signal { 0 }

    val parentA = Var(Map(root -> 2))
    val WeightA = mini(parentA)

    assert(WeightA.readValueOnce == 2)

    val parentB = Var(Map(root -> 1))
    val WeightB = mini(parentB)

    assert(WeightB.readValueOnce == 1)

    val parentC = Var(Map(WeightA -> 3, WeightB -> 10))
    val WeightC = mini(parentC)

    assert(WeightC.readValueOnce == 5)

    parentC.transform(_ + (WeightB -> 1))

    assert(WeightC.readValueOnce == 2)

  }

}
