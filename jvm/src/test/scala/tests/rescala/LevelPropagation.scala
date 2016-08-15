package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.Infiltrator.assertLevel
import rescala.engines.Engine
import rescala.graph.LevelStruct
import rescala.propagation.Turn
import rescala.reactives.Signals

object LevelPropagation extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class LevelPropagation[S <: LevelStruct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit  {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.Var

  @Test def worksOnElementsInQueue(): Unit = {
    val level0 = Var(0)
    val l1 = level0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val level3 = l2.map(_ + 1)
    val level_1_to_4 = Signals.dynamic(level0) { t =>
      if (level0(t) == 10) level3(t) else 42
    }
    var evaluatesOnlyOncePerTurn = 0
    val level_2_to_5 = Signals.lift(level0, level_1_to_4) { (x, y) => evaluatesOnlyOncePerTurn += 1; x + y }

    assertLevel(level3, 3)
    assertLevel(level_1_to_4, 1)
    assertLevel(level_2_to_5, 2)
    assert(level_2_to_5.now === 42)
    assert(evaluatesOnlyOncePerTurn === 1)

    level0.set(5)

    assertLevel(level3, 3)
    assertLevel(level_1_to_4, 1)
    assertLevel(level_2_to_5, 2)
    assert(level_2_to_5.now === 47)
    assert(evaluatesOnlyOncePerTurn === 2)

    level0.set(10)

    assertLevel(level3, 3)
    assertLevel(level_1_to_4, 4)
    assertLevel(level_2_to_5, 5)
    assert(level_2_to_5.now === 23)
    assert(evaluatesOnlyOncePerTurn === 3)


  }

  @Test def doesNotBreakStuffWhenNothingChangesBeforeDependenciesAreCorrect(): Unit = {
    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signals.dynamic(l0) { t =>
      if (l0(t) == 10) l3(t) else 3
    }
    val l2t5 = l1t4.map(_ + 1)

    assertLevel(l3, 3)
    assertLevel(l1t4, 1)
    assertLevel(l2t5, 2)
    assert(l1t4.now === 3)
    assert(l2t5.now === 4)


    l0.set(10)

    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assert(l1t4.now === 13)
    assert(l2t5.now === 14)

  }

  @Test def doesNotReevaluateStuffIfNothingChanges(): Unit = {
    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signals.dynamic(l0) { t =>
      if (l0(t) == 10) l3(t) else 13
    }
    var reevals = 0
    val l2t5 = l1t4.map { v => reevals += 1; v + 1 }

    assertLevel(l3, 3)
    assertLevel(l1t4, 1)
    assertLevel(l2t5, 2)
    assert(l1t4.now === 13)
    assert(l2t5.now === 14)
    assert(reevals === 1)


    l0.set(10)

    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assert(l0.now === 10)
    assert(l1.now === 11)
    assert(l2.now === 12)
    assert(l3.now === 13)
    assert(l1t4.now === 13)
    assert(l2t5.now === 14)
    assert(reevals === 1)


  }

  @Test def levelIncreaseAndChangeFromeSomewhereElseWorksTogether(): Unit = {
    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signals.dynamic(l0) { t =>
      if (l0(t) == 10) l3(t) else 13
    }
    var reevals = 0
    val l2t5 = Signals.lift(l1t4, l1) { (a, b) => reevals += 1; a + b }
    var reevals2 = 0
    val l3t6 = l2t5.map { v => reevals2 += 1; v + 1 }

    assertLevel(l3, 3)
    assertLevel(l1t4, 1)
    assertLevel(l2t5, 2)
    assert(l1t4.now === 13)
    assert(l2t5.now === 14)
    assert(reevals === 1)
    assert(reevals2 === 1)


    l0.set(10)

    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assert(l1t4.now === 13)
    assert(l2t5.now === 24)
    assert(reevals === 2)
    assert(reevals2 === 2)


  }

}
