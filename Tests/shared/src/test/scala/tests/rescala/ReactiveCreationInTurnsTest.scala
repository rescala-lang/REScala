package tests.rescala




class ReactiveCreationInTurnsTest extends RETests {




  allEngines("evaluations Of Inner Signals"){ engine => import engine._

    val v1 = Var(5)
    val c1 = Var(0)
    val v2 = v1.map { x =>
      var res = 0
      c1.map(x => {res += 1; x})
      res
    }

    assert(v2.now === 1, "unrelated signal should only be evaluated once on creation")

    v1.set(100)

    assert(v2.now === 1, "unrelated signal should only be evaluated once on change")

  }

  allEngines("evaluations Of Inner Related Signals"){ engine => import engine._

    val v1 = Var(5)
    val v2 = v1.map { x =>
      var res = 0
      v1.map(x => {res += 1; x})
      res
    }

    assert(v2.now === 1, "related signal is only be evaluated once on creation (this behaviour is actually undefined)")

    v1.set(100)

    assert(v2.now === 2, "related signal should be evaluated twice on change (this behaviour is actually undefined)")

  }


  allEngines("change Of Created Signal"){ engine => import engine._

    engine.transaction() { implicit t =>
      val v1 = rescala.reactives.Var(0)
      val v2 = v1.map(_ + 1)
      val c1 = v1.change.observe(v => assert(false, s"created signals do not change, but change was $v"))
      val c2 = v2.change.observe(v => assert(false, s"created mapped signals do not change, but change was $v"))
    }

    {
      val v1 = Var(0)
      var a1 = false
      var a2 = false
      var v2: Signal[Int] = null
      implicitEngine.transaction(v1) { implicit t =>
        v2 = v1.map(_ + 1)
        val c1 = v1.change
        c1.observe(_ => a1 = true)
        val c2 = v2.change
        c2.observe(_ => a2 = true)
        v1.admit(10)
      }
      assert(a1, "created signals do not change when admitting in same turn")
      assert(a2, "created mapped signals do not change when admitting in same turn")
      assert(v1.now == 10)
      assert(v2.now == 11)
    }

    {
      val v1 = Var(0)
      val v2 = v1.map(_ + 1)
      var o1 = false
      var o2 = false
      val c1 = v1.change.observe(_ => o1 = true)
      val c2 = v2.change.observe(_ => o2 = true)
      assert(!o1, "created signals do not change outside of turn during creation")
      assert(!o2, "created mapped signals do not change outside of turn during creation")
      v1.set(10)
      assert(o1, "created signals do change outside of turn")
      assert(o2, "created mapped signals do change outside of turn")
    }

  }


}
