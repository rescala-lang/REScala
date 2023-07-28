package test.kofre

import kofre.time.{ArrayRanges, Dots, VectorClock}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

import scala.math.Ordering.Implicits.infixOrderingOps

class VectorClockOrderTest
    extends OrderTests[VectorClock](total = true)(using VectorClock.vectorClockTotalOrdering, summon)
class ArrayRangesOrderTest extends OrderTests[ArrayRanges](total = false)(using ArrayRanges.partialOrder, summon)
class DotsOrderTest extends OrderTests[Dots](total = false)(using Dots.partialOrder, summon)

// the specification of these tests is nice, but the generators are essentially useless, as it is extremely unlikely
// that they will produce any kind of comparable values
abstract class OrderTests[A: PartialOrdering: Arbitrary](total: Boolean) extends munit.ScalaCheckSuite {

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

  property("reflxive") {
    forAll { (a: A) =>
      assert(a <= a)
    }
  }
}
