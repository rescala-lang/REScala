package tests.rescala.misc

import tests.rescala.testtools.FunSuiteInvertedAssert

class ReactiveCreationInTurnsTest extends munit.FunSuite {
  import reactives.default.*
  {

    test("evaluations Of Inner Signals") {

      val v1 = Var(5)
      val c1 = Var(0)
      val v2 = Signal {
        val _   = v1.value
        var res = 0
        c1.map(x => { res += 1; x })
        res
      }

      assertEquals(v2.readValueOnce, 1, "unrelated signal should only be evaluated once on creation")

      v1.set(100)

      assertEquals(v2.readValueOnce, 1, "unrelated signal should only be evaluated once on change")

    }

    test("evaluations Of Inner Related Signals") {

      val v1 = Var(5)
      val v2 = Signal {
        val _   = v1.value
        var res = 0
        v1.map(x => { res += 1; x })
        res
      }

      assertEquals(
        1,
        v2.readValueOnce,
        "related signal is only be evaluated once on creation (this behaviour is actually undefined)"
      )

      v1.set(100)

      assertEquals(
        1,
        v2.readValueOnce,
        "related signal should be evaluated once on change (this behaviour is actually undefined)"
      )

    }

    test("change Of Created Signal") {

      transaction() {
        val v1 = Var(0)
        val v2 = v1.map(_ + 1)
        v1.change.observe(v => fail(s"created signals should not change, but change was $v"))
        v2.change.observe(v => fail(s"created mapped signals should not change, but change was $v"))
      }

//    {
//      val v1 = Var(0)
//      var v2: Signal[Int] = null
//      var v1changedFired = false
//      implicitEngine.transaction(v1) { implicit t =>
//        val c1 = v1.change
//        c1.observe(v => v1changedFired = true)
//        v2 = v1.map(_ + 1)
//        val c2 = v2.change
//        c2.observe(v => fail("created mapped signals should not change when admitting in same turn, but change was " + v))
//        v1.admit(10)
//      }
//      assert(v1changedFired, "created change events should fire when admitting in same turn, but did not.")
//      assert(v1.now == 10)
//      assert(v2.now == 11)
//    }

      {
        val v1 = Var(0)
        val v2 = v1.map(_ + 1)
        var o1 = false
        var o2 = false
        v1.change.observe(_ => o1 = true)
        v2.change.observe(_ => o2 = true)
        assert(!o1, "created signals do not change outside of turn during creation")
        assert(!o2, "created mapped signals do not change outside of turn during creation")
        v1.set(10)
        assert(o1, "created signals do change outside of turn")
        assert(o2, "created mapped signals do change outside of turn")
      }

    }

//  test("create changes during reevaluation"){
//    val v = Var(1)
//    val mapped = v.map(_ + 0)
//
//    val sm = Signal { mapped.change.apply() }
//    val sd = dynamic() {t => t.depend(mapped.change(CreationTicket.fromTicketDImplicit(t, implicitly))) }
//
//
//    //intercept[NoSuchElementException](sm.now)
//    //assert(sm.now.isEmpty)
//    //assert(sd.now.isEmpty)
//
//    v.set(2)
//
//    assert(sm.now.get.pair == 1 -> 2)
//    assert(sd.now.get.pair == 1 -> 2)
//
//    v.set(3)
//
//    assert(sm.now.get.pair == 2 -> 3)
//    assert(sd.now.get.pair == 2 -> 3)
//
//  }

  }
}
