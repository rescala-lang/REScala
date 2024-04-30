package test.rdts.baseproperties

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import rdts.time.{ArrayRanges, Dots, VectorClock}
import test.rdts.DataGenerator.given

import scala.collection.immutable.Queue

class VectorClockTotalOrderTest
    extends OrderTests[VectorClock](using VectorClock.vectorClockTotalOrdering)(total = true, agreesWithEquals = true)
class VectorClockOrderTest
    extends OrderTests[VectorClock](using VectorClock.vectorClockOrdering)(total = false, agreesWithEquals = true)
class ArrayRangesOrderTest
    extends OrderTests[ArrayRanges](using ArrayRanges.partialOrder)(total = false, agreesWithEquals = true)
class DotsOrderTest extends OrderTests[Dots](using Dots.partialOrder)(total = false, agreesWithEquals = true)

// the specification of these tests is nice, but the generators are essentially useless, as it is extremely unlikely
// that they will produce any kind of comparable values
abstract class OrderTests[A: Arbitrary](using pa: PartialOrdering[A])(total: Boolean, agreesWithEquals: Boolean)
    extends munit.ScalaCheckSuite {

  extension [A](using pa: PartialOrdering[A])(a: A)
    infix def <(b: A)     = pa.lt(a, b)
    infix def <=(b: A)    = pa.lteq(a, b)
    infix def equiv(b: A) = pa.equiv(a, b)

  property("transitive") {
    forAll { (a: A, b: A, c: A) =>
      if a < b && b < c then assert(a < c)
    }
  }

  property("antisymmetric") {
    forAll { (a: A, b: A) =>
      if a <= b && b <= a
      then
        assert(a equiv b)
        if agreesWithEquals then assert(a == b)
    }
  }

  property("total") {
    if total
    then
      forAll { (a: A, b: A) =>
        assert(a <= b || b <= a)
      }
  }

  property("reflexive") {
    forAll { (a: A) =>
      assert(a <= a)
    }
  }

  property("order commutative") {
    forAll: (a: A, b: A) =>
      val left  = pa.tryCompare(a, b)
      val right = pa.tryCompare(b, a)
      // negation is a bit weird in the sense that “not comparable” negates to “not comparable”
      val invertedRight = right.map(x => -x)
      assertEquals(left, invertedRight, s"a: $a\nb: $b")
  }

  property("sorts") {
    pa match
      // this both checks that pa is a total order and that the total flag is set
      case given Ordering[A] if total =>
        forAll: (list: List[A]) =>
          list.to(Queue).sorted == list.to(Queue).sorted.sorted
      case other =>
  }
}
