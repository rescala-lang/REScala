package test.rdts.bespoke

import rdts.base.Uid
import rdts.time.{ArrayRanges, Dots, VectorClock}

class OrderRegressionTets extends munit.ScalaCheckSuite {

  // don’t quite remember what this was a regression for.
  test("Vector Sorting"):
    val a = VectorClock(Map(Uid.predefined("a") -> 1522394954533558658L))
    val b = VectorClock(
      Map(Uid.predefined("d") -> 3422060934355809334L)
    )

    given Ordering[VectorClock] = VectorClock.vectorClockTotalOrdering

    assertEquals(List(a, b).sorted, List(b, a).sorted.sorted)

  // ArrayRanges ordering was buggy
  test("dots order commutative"):
    val a = Dots(Map(Uid.predefined("a") -> ArrayRanges.from(List(1))))
    val b = Dots(Map(
      Uid.predefined("a") -> ArrayRanges.from(List(2)),
      Uid.predefined("c") -> ArrayRanges.from(List(3)),
    ))
    val left  = Dots.partialOrder.tryCompare(a, b)
    val right = Dots.partialOrder.tryCompare(b, a)
    assertEquals(left, right.map(x => -x), s"a: $a\nb: $b")
}
