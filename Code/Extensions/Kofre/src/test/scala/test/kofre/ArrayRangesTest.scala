package test.kofre

import kofre.causality.ArrayRanges
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrayRangesTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "contains works" in {
    val a = ArrayRanges(Seq[(Long, Long)]((5, 10), (20, 40)))
    List(Range(5, 10), Range(20, 40)).flatten.foreach { i => assert(a.contains(i)) }
    List(Range(10, 20), Range(40, 50)).flatten.foreach { i => assert(!a.contains(i)) }
  }

  "merge" in {

    val empty = ArrayRanges.empty

    val a = ArrayRanges(Seq[(Long, Long)]((5, 10), (20, 40)))
    val b = ArrayRanges(Seq[(Long, Long)]((10, 20)))

    assert(empty.merge(b) == b)
    assert(b.merge(empty) == b)

    assert(a.merge(b) == ArrayRanges(Seq[(Long, Long)]((5, 40))))
  }

}
