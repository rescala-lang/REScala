package test.rdts.baseproperties

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Uid
import rdts.time.{ArrayRanges, Dots, VectorClock}
import test.rdts.DataGenerator.{*, given}

import scala.collection.immutable.Queue
import scala.math.Ordering.Implicits.infixOrderingOps

class VectorClockTotalOrderTest
    extends OrderTests[VectorClock](using VectorClock.vectorClockTotalOrdering)(total = true)
class VectorClockOrderTest
    extends OrderTests[VectorClock](using VectorClock.vectorClockOrdering)(total = false)
class ArrayRangesOrderTest extends OrderTests[ArrayRanges](using ArrayRanges.partialOrder)(total = false)
class DotsOrderTest        extends OrderTests[Dots](using Dots.partialOrder)(total = false)

// the specification of these tests is nice, but the generators are essentially useless, as it is extremely unlikely
// that they will produce any kind of comparable values
abstract class OrderTests[A: Arbitrary](using pa: PartialOrdering[A])(total: Boolean) extends munit.ScalaCheckSuite {

  extension [A](using pa: PartialOrdering[A])(a: A)
    infix def <(b: A)  = pa.lt(a, b)
    infix def <=(b: A) = pa.lteq(a, b)

  property("transitive") {
    forAll { (a: A, b: A, c: A) =>
      if a < b && b < c then assert(a < c)
    }
  }

  property("antisymmetric") {
    forAll { (a: A, b: A) =>
      if a <= b && b <= a then assert(a == b)
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

  property("order commutative"):
    forAll: (a: A, b: A) =>
      val left  = pa.tryCompare(a, b)
      val right = pa.tryCompare(b, a)
      assertEquals(left, right.map(x => -x), s"a: $a\nb: $b")

  property("sorts"):
    if total
    then
      given Ordering[A] = pa.asInstanceOf[Ordering[A]]
      forAll: (list: List[A]) =>
        list.to(Queue).sorted == list.to(Queue).sorted.sorted
}


