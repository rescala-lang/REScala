package test.kofre

import kofre.primitives.{LastWriterWins, MultiValueRegister}
import kofre.sets.ORSet
import kofre.{Defs, Lattice}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import test.kofre.DataGenerator.{*, given}
import kofre.causality.VectorClock

class VectorClockLattice extends LatticeMergeTest[VectorClock]
class LWWLatice          extends LatticeMergeTest[LastWriterWins[Int]]
class OrSetLatice        extends LatticeMergeTest[ORSet[Int]]
class MVRLattice         extends LatticeMergeTest[MultiValueRegister[Int]]

abstract class LatticeMergeTest[A: Arbitrary: Lattice] extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "idempotent" in forAll { (a: A, b: A) =>
    val ab  = Lattice.merge(a, b)
    val abb = Lattice.merge(ab, b)
    assert(ab === abb)
  }

  "commutative" in forAll { (a: A, b: A) =>
    assert(Lattice.merge(b, a) === Lattice.merge(a, b))
  }

  "associative" in forAll { (a: A, b: A, c: A) =>
    val ab   = Lattice.merge(a, b)
    val bc   = Lattice.merge(b, c)
    val abc  = Lattice.merge(ab, c)
    val abc2 = Lattice.merge(a, bc)
    assert(abc === abc2)
  }

}
