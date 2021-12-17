package tests.rescala.misc

import rescala.core.infiltration.Infiltrator
import tests.rescala.testtools.RETests
import rescala.interface.RescalaInterface
import rescala.scheduler.Levelbased

class LevelPropagation extends RETests {
  multiEngined { engine =>
    val ie = new Infiltrator(engine.asInstanceOf[RescalaInterface with Levelbased])
    import ie.api._
    import ie.assertLevel

    test("works On Elements In Queue") {
      val level0 = Var(0)
      val l1     = level0.map(_ + 1)
      val l2     = l1.map(_ + 1)
      val level3 = l2.map(_ + 1)
      assert(level0.readValueOnce === 0)
      assert(l1.readValueOnce === 1)
      assert(l2.readValueOnce === 2)
      assert(level3.readValueOnce === 3)
      val level_1_to_4 = Signals.dynamic(level0) { t =>
        if (t.depend(level0) == 10) t.depend(level3) else 42
      }
      assert(level_1_to_4.readValueOnce === 42)
      var evaluatesOnlyOncePerTurn = 0
      val level_2_to_5 = Signals.lift(level0, level_1_to_4) { (x, y) =>
        evaluatesOnlyOncePerTurn += 1; x + y
      }
      assert(level_2_to_5.readValueOnce === 0 + 42)

      assertLevel(level3, 3)
      assertLevel(level_1_to_4, 1)
      assertLevel(level_2_to_5, 2)
      assert(level_2_to_5.readValueOnce === 42)
      assert(evaluatesOnlyOncePerTurn === 1)

      level0.set(5)

      assertLevel(level3, 3)
      assertLevel(level_1_to_4, 1)
      assertLevel(level_2_to_5, 2)
      assert(level_2_to_5.readValueOnce === 47)
      assert(evaluatesOnlyOncePerTurn === 2)

      level0.set(10)

      assertLevel(level3, 3)
      assertLevel(level_1_to_4, 4)
      assertLevel(level_2_to_5, 5)
      assert(level_2_to_5.readValueOnce === 23)
      assert(evaluatesOnlyOncePerTurn === 3)

    }

    test("does Not Break Stuff When Nothing Changes Before Dependencies Are Correct") {
      val l0 = Var(0)
      val l1 = l0.map(_ + 1)
      val l2 = l1.map(_ + 1)
      val l3 = l2.map(_ + 1)
      val l1t4 = Signals.dynamic(l0) { t =>
        if (t.depend(l0) == 10) t.depend(l3) else 3
      }
      val l2t5 = l1t4.map(_ + 1)

      assertLevel(l3, 3)
      assertLevel(l1t4, 1)
      assertLevel(l2t5, 2)
      assert(l1t4.readValueOnce === 3)
      assert(l2t5.readValueOnce === 4)

      l0.set(10)

      assertLevel(l3, 3)
      assertLevel(l1t4, 4)
      assertLevel(l2t5, 5)
      assert(l1t4.readValueOnce === 13)
      assert(l2t5.readValueOnce === 14)

    }

    test("does Not Reevaluate Stuff If Nothing Changes") {
      val l0 = Var(0)
      val l1 = l0.map(_ + 1)
      val l2 = l1.map(_ + 1)
      val l3 = l2.map(_ + 1)
      val l1t4 = Signals.dynamic(l0) { t =>
        if (t.depend(l0) == 10) t.depend(l3) else 13
      }
      var reevals = 0
      val l2t5 = l1t4.map { v =>
        reevals += 1; v + 1
      }
      // in the initial state t1t4 depends on 20 which is not 10, so the result is 14

      assertLevel(l3, 3)
      assertLevel(l1t4, 1)
      assertLevel(l2t5, 2)
      assert(l1t4.readValueOnce === 13)
      assert(l2t5.readValueOnce === 14)
      assert(reevals === 1)

      // changing l0 to 10 will update the dependencies of l1t4 to include l3, but that is 13 which is the same value
      // as before, so the map is not executed again.

      l0.set(10)
      assert(reevals === 1)
      assertLevel(l3, 3)
      assertLevel(l1t4, 4)
      assertLevel(l2t5, 5)
      assert(l0.readValueOnce === 10)
      assert(l1.readValueOnce === 11)
      assert(l2.readValueOnce === 12)
      assert(l3.readValueOnce === 13)
      assert(l1t4.readValueOnce === 13)
      assert(l2t5.readValueOnce === 14)

    }

    test("level Increase And Change From Somewhere Else Works Together") {
      val l0 = Var(0)
      val l1 = l0.map(_ + 1)
      val l2 = l1.map(_ + 1)
      val l3 = l2.map(_ + 1)
      val l1t4 = Signals.dynamic(l0) { t =>
        if (t.depend(l0) == 10) t.depend(l3) else 13
      }
      var reevals = 0
      val l2t5 = Signals.lift(l1t4, l1) { (a, b) =>
        reevals += 1; a + b
      }
      var reevals2 = 0
      val l3t6 = l2t5.map { v =>
        reevals2 += 1; v + 1
      }

      assertLevel(l3, 3)
      assertLevel(l1t4, 1)
      assertLevel(l2t5, 2)
      assertLevel(l3t6, 3)
      assert(l1t4.readValueOnce === 13)
      assert(l2t5.readValueOnce === 14)
      assert(reevals === 1)
      assert(reevals2 === 1)

      l0.set(10)

      assertLevel(l3, 3)
      assertLevel(l1t4, 4)
      assertLevel(l2t5, 5)
      assert(l1t4.readValueOnce === 13)
      assert(l2t5.readValueOnce === 24)
      assert(reevals === 2)
      assert(reevals2 === 2)

    }

    test("level increase but value change only after correct level is reached") {

      val l0 = Var(0)
      val l1 = l0.map(_ + 1)
      val l2 = l1.map(_ + 1)
      val l3 = l2.map(_ + 1)
      val l1t4 = Signals.dynamic(l0) { t =>
        if (t.depend(l0) == 10) t.depend(l3) else 3
      }
      var reevals = 0
      val l2t5 = l1t4.map { v =>
        reevals += 1; v + 1
      }
      // in the initial state t1t4 depends on 20 which is not 10, so the result is 14

      assertLevel(l3, 3)
      assertLevel(l1t4, 1)
      assertLevel(l2t5, 2)
      assert(l1t4.readValueOnce === 3)
      assert(l2t5.readValueOnce === 4)
      assert(reevals === 1)

      // changing l0 to 10 will update the dependencies of l1t4 to include l3, but that is 13 which is the same value
      // as before, so the map is not executed again.

      l0.set(10)
      assert(reevals === 2)
      assertLevel(l3, 3)
      assertLevel(l1t4, 4)
      assertLevel(l2t5, 5)
      assert(l0.readValueOnce === 10)
      assert(l1.readValueOnce === 11)
      assert(l2.readValueOnce === 12)
      assert(l3.readValueOnce === 13)
      assert(l1t4.readValueOnce === 13)
      assert(l2t5.readValueOnce === 14)

    }

  }
}
