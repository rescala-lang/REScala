package test.kofre

import kofre.base.{Time, Lattice}
import kofre.datatypes.ObserveRemoveSet
import kofre.time.VectorClock
import kofre.primitives.{CausalQueue, LastWriterWins, MultiValueRegister}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

class VectorClockLattice extends LatticeMergeTest[VectorClock]
class LWWLatice          extends LatticeMergeTest[LastWriterWins[Time, Int]]
class OrSetLatice        extends LatticeMergeTest[ObserveRemoveSet[Int]]
class MVRLattice         extends LatticeMergeTest[MultiValueRegister[Int]]

abstract class LatticeMergeTest[A: Arbitrary: Lattice] extends munit.ScalaCheckSuite {

  property("idempotent") {
    forAll { (a: A, b: A) =>
      val ab  = Lattice.merge(a, b)
      val abb = Lattice.merge(ab, b)
      assertEquals(ab, abb)
    }
  }
  property("commutative") {
    forAll { (a: A, b: A) =>
      assertEquals(Lattice.merge(b, a), Lattice.merge(a, b))
    }
  }
  property("associative") {
    forAll { (a: A, b: A, c: A) =>
      val ab   = Lattice.merge(a, b)
      val bc   = Lattice.merge(b, c)
      val abc  = Lattice.merge(ab, c)
      val abc2 = Lattice.merge(a, bc)
      assertEquals(abc, abc2)
    }
  }
}
