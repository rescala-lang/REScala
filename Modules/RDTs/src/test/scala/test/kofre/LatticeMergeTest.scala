package test.kofre

import kofre.base.{Lattice, Time}
import kofre.datatypes.alternatives.lww.GenericLastWriterWins
import kofre.datatypes.alternatives.{MultiValueRegister, ObserveRemoveSet}
import kofre.datatypes.contextual.{CausalQueue, LastWriterWins}
import kofre.dotted.{Dotted, HasDots}
import kofre.time.{CausalityException, Dots, VectorClock}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

import scala.util.NotGiven

class VectorClockLattice extends LatticeMergeTest[VectorClock]
class LWWLatice          extends LatticeMergeTest[GenericLastWriterWins[Time, Int]]
class OrSetLatice        extends LatticeMergeTest[ObserveRemoveSet[Int]]
class LWWLattice         extends LatticeMergeTest[Dotted[LastWriterWins[Int]]]
class MVRLattice         extends LatticeMergeTest[MultiValueRegister[Int]]

abstract class LatticeMergeTest[A: Arbitrary: Lattice]
    extends munit.ScalaCheckSuite {

  /** because examples are generated independently, they sometimes produce causally inconsistent results */
  inline def ignoreCausalErrors[A](inline expr: Unit): Unit =
    try expr
    catch case _: CausalityException => ()

  property("idempotent") {
    forAll { (a: A, b: A) =>
      ignoreCausalErrors:
        val ab  = Lattice.merge(a, b)
        val abb = Lattice.merge(ab, b)
        assertEquals(ab, abb)

    }
  }
  property("commutative") {
    forAll { (a: A, b: A) =>
      ignoreCausalErrors:
        assertEquals(Lattice.merge(b, a), Lattice.merge(a, b))
    }
  }
  property("associative") {
    forAll { (a: A, b: A, c: A) =>
      ignoreCausalErrors:
        val ab   = Lattice.merge(a, b)
        val bc   = Lattice.merge(b, c)
        val abc  = Lattice.merge(ab, c)
        val abc2 = Lattice.merge(a, bc)
        assertEquals(abc, abc2)
    }
  }
}

/** Not used, was implemented to filter out duplicates, but seems too complicated. */
class DistinctMachinery[A](using maybeHasDots: Option[HasDots[A]]) {

  /** In the case that [[A]] has a [[kofre.dotted.DottedLattice]], merging of two randomly generated values may not be commutative/associative, because */
  def distinct(a: A, b: A): Boolean =
    maybeHasDots match
      case Some(hd) =>
        hd.dots(a) disjunct hd.dots(b)
      case None =>
        true

  private inline given hasDotsIfPresent[A](using NotGiven[A =:= Dotted[_]]): Option[HasDots[A]] =
    scala.compiletime.summonFrom:
      case hd: HasDots[A] => Some(hd)
      case _              => None

  private inline given hasDotsIfPresent[A]: Option[HasDots[Dotted[A]]] =
    scala.compiletime.summonFrom:
      case hd: HasDots[A] =>
        Some(new HasDots[Dotted[A]] {
          extension (dotted: Dotted[A])
            def dots: Dots                                = hd.dots(dotted.data)
            def removeDots(dots: Dots): Option[Dotted[A]] = None
        })
      case _ =>
        None
}
