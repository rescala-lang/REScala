package test.kofre

import kofre.time.VectorClock
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}
import math.Ordering.Implicits.infixOrderingOps

class VectorClockOrderTest extends OrderTests[VectorClock](using VectorClock.vectorClockTotalOrdering, summon)

abstract class OrderTests[A: Ordering: Arbitrary] extends munit.ScalaCheckSuite {

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
