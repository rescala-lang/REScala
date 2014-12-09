package tests.rescala

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.interfaces.Engines
import rescala.{Signals, Var}
import Engines.default

class LevelPropagation extends AssertionsForJUnit {

  @Test def worksOnElementsInQueue(): Unit = {
    val level0 = Var(0)
    val l1 = level0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val level3 = l2.map(_ + 1)
    val level_1_to_4 = Signals.dynamic(level0) { t =>
      if (level0(t) == 10) level3(t) else 42
    }
    var evaluatesOnlyOncePerTurn = 0
    val level_2_to_5 = Signals.lift(level0, level_1_to_4){(x, y) => evaluatesOnlyOncePerTurn += 1 ;  x + y}

    assert(level3.getLevel === 3)
    assert(level_1_to_4.getLevel === 1)
    assert(level_2_to_5.getLevel === 2)
    assert(level_2_to_5.now === 42)
    assert(evaluatesOnlyOncePerTurn === 1)

    level0.set(5)

    assert(level3.getLevel === 3)
    assert(level_1_to_4.getLevel === 1)
    assert(level_2_to_5.getLevel === 2)
    assert(level_2_to_5.now === 47)
    assert(evaluatesOnlyOncePerTurn === 2)

    level0.set(10)

    assert(level3.getLevel === 3)
    assert(level_1_to_4.getLevel === 4)
    assert(level_2_to_5.getLevel === 5)
    assert(level_2_to_5.now === 23)
    assert(evaluatesOnlyOncePerTurn === 3)


  }

}
