package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Infiltrator.assertLevel
import rescala.engines.Engine
import rescala.graph.LevelStruct
import rescala.propagation.Turn
import rescala.reactives.Signals

object ReactiveCreationInTurnsTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class ReactiveCreationInTurnsTest[S <: LevelStruct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine

  import implicitEngine.Var

  @Test def evaluationsOfInnerSignals(): Unit = {

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

  @Test def evaluationsOfInnerRelatedSignals(): Unit = {

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


  @Test def changeOfCreatedSignal(): Unit = {

    engine.plan() { implicit t =>
      val v1 = rescala.reactives.Var(0)
      val v2 = v1.map(_ + 1)
      val c1 = v1.change.observe(_ => assert(false, "created signals do not change"))
      val c2 = v2.change.observe(_ => assert(false, "created mapped signals do not change"))
    }

    {
      val v1 = Var(0)
      engine.plan() { implicit t =>
        val v2 = v1.map(_ + 1)
        val c1 = v1.change
        c1.observe(_ => assert(false, "created signals do not change when admitting in same turn"))
        val c2 = v2.change
        c2.observe(_ => assert(false, "created mapped signals do not change when admitting in same turn"))
        v1.admit(10)
      }
      assert(v1.now == 10)
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
