package tests.rescala.misc

import reactives.core.infiltration.Infiltrator
import reactives.default.*
import tests.rescala.testtools.FunSuiteInvertedAssert

class LevelPropagation extends FunSuiteInvertedAssert {
  val ie = new Infiltrator()
  import ie.assertLevel

  test("works On Elements In Queue") {
    val level0 = Var(0)
    val l1     = level0.map(_ + 1)
    val l2     = l1.map(_ + 1)
    val level3 = l2.map(_ + 1)
    assertEquals(level0.readValueOnce, 0)
    assertEquals(l1.readValueOnce, 1)
    assertEquals(l2.readValueOnce, 2)
    assertEquals(level3.readValueOnce, 3)
    val level_1_to_4 = Signal.dynamic(level0) { t =>
      if t.depend(level0) == 10 then t.depend(level3) else 42
    }
    assertEquals(level_1_to_4.readValueOnce, 42)
    var evaluatesOnlyOncePerTurn = 0
    val level_2_to_5 = Signal.lift(level0, level_1_to_4) { (x, y) =>
      evaluatesOnlyOncePerTurn += 1; x + y
    }
    assertEquals(level_2_to_5.readValueOnce, 0 + 42)

    assertLevel(level3, 3)
    assertLevel(level_1_to_4, 1)
    assertLevel(level_2_to_5, 2)
    assertEquals(level_2_to_5.readValueOnce, 42)
    assertEquals(evaluatesOnlyOncePerTurn, 1)

    level0.set(5)

    assertLevel(level3, 3)
    assertLevel(level_1_to_4, 1)
    assertLevel(level_2_to_5, 2)
    assertEquals(level_2_to_5.readValueOnce, 47)
    assertEquals(evaluatesOnlyOncePerTurn, 2)

    level0.set(10)

    assertLevel(level3, 3)
    assertLevel(level_1_to_4, 4)
    assertLevel(level_2_to_5, 5)
    assertEquals(level_2_to_5.readValueOnce, 23)
    assertEquals(evaluatesOnlyOncePerTurn, 3)

  }

  test("does Not Break Stuff When Nothing Changes Before Dependencies Are Correct") {
    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signal.dynamic(l0) { t =>
      if t.depend(l0) == 10 then t.depend(l3) else 3
    }
    val l2t5 = l1t4.map(_ + 1)

    assertLevel(l3, 3)
    assertLevel(l1t4, 1)
    assertLevel(l2t5, 2)
    assertEquals(l1t4.readValueOnce, 3)
    assertEquals(l2t5.readValueOnce, 4)

    l0.set(10)

    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assertEquals(l1t4.readValueOnce, 13)
    assertEquals(l2t5.readValueOnce, 14)

  }

  test("does Not Reevaluate Stuff If Nothing Changes") {
    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signal.dynamic(l0) { t =>
      if t.depend(l0) == 10 then t.depend(l3) else 13
    }
    var reevals = 0
    val l2t5 = l1t4.map { v =>
      reevals += 1; v + 1
    }
    // in the initial state t1t4 depends on 20 which is not 10, so the result is 14

    assertLevel(l3, 3)
    assertLevel(l1t4, 1)
    assertLevel(l2t5, 2)
    assertEquals(l1t4.readValueOnce, 13)
    assertEquals(l2t5.readValueOnce, 14)
    assertEquals(reevals, 1)

    // changing l0 to 10 will update the dependencies of l1t4 to include l3, but that is 13 which is the same value
    // as before, so the map is not executed again.

    l0.set(10)
    assertEquals(reevals, 1)
    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assertEquals(l0.readValueOnce, 10)
    assertEquals(l1.readValueOnce, 11)
    assertEquals(l2.readValueOnce, 12)
    assertEquals(l3.readValueOnce, 13)
    assertEquals(l1t4.readValueOnce, 13)
    assertEquals(l2t5.readValueOnce, 14)

  }

  test("level Increase And Change From Somewhere Else Works Together") {
    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signal.dynamic(l0) { t =>
      if t.depend(l0) == 10 then t.depend(l3) else 13
    }
    var reevals = 0
    val l2t5 = Signal.lift(l1t4, l1) { (a, b) =>
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
    assertEquals(l1t4.readValueOnce, 13)
    assertEquals(l2t5.readValueOnce, 14)
    assertEquals(reevals, 1)
    assertEquals(reevals2, 1)

    l0.set(10)

    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assertEquals(l1t4.readValueOnce, 13)
    assertEquals(l2t5.readValueOnce, 24)
    assertEquals(reevals, 2)
    assertEquals(reevals2, 2)

  }

  test("level increase but value change only after correct level is reached") {

    val l0 = Var(0)
    val l1 = l0.map(_ + 1)
    val l2 = l1.map(_ + 1)
    val l3 = l2.map(_ + 1)
    val l1t4 = Signal.dynamic(l0) { t =>
      if t.depend(l0) == 10 then t.depend(l3) else 3
    }
    var reevals = 0
    val l2t5 = l1t4.map { v =>
      reevals += 1; v + 1
    }
    // in the initial state t1t4 depends on 20 which is not 10, so the result is 14

    assertLevel(l3, 3)
    assertLevel(l1t4, 1)
    assertLevel(l2t5, 2)
    assertEquals(l1t4.readValueOnce, 3)
    assertEquals(l2t5.readValueOnce, 4)
    assertEquals(reevals, 1)

    // changing l0 to 10 will update the dependencies of l1t4 to include l3, but that is 13 which is the same value
    // as before, so the map is not executed again.

    l0.set(10)
    assertEquals(reevals, 2)
    assertLevel(l3, 3)
    assertLevel(l1t4, 4)
    assertLevel(l2t5, 5)
    assertEquals(l0.readValueOnce, 10)
    assertEquals(l1.readValueOnce, 11)
    assertEquals(l2.readValueOnce, 12)
    assertEquals(l3.readValueOnce, 13)
    assertEquals(l1t4.readValueOnce, 13)
    assertEquals(l2t5.readValueOnce, 14)

  }

}
