package test.kofre.baseproperties

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.alternatives.{MultiValueRegister, ObserveRemoveSet}
import kofre.datatypes.contextual.CausalQueue
import kofre.datatypes.{GrowOnlyCounter, GrowOnlyList, GrowOnlyMap, LastWriterWins, PosNegCounter, TwoPhaseSet}
import kofre.dotted.{DotMap, DotSet, Dotted, DottedLattice, HasDots}
import kofre.time.{CausalityException, Dots, Time, VectorClock}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

import scala.util.NotGiven

class DotSetChecks          extends LatticePropertyChecks[Dotted[DotSet]]
class DotMapChecks          extends LatticePropertyChecks[Dotted[DotMap[kofre.base.Uid, DotSet]]]
class GrowOnlyCounterChecks extends LatticePropertyChecks[GrowOnlyCounter]
class GrowOnlyMapChecks     extends LatticePropertyChecks[GrowOnlyMap[String, Int]]
class TwoPhaseSetChecks     extends LatticePropertyChecks[TwoPhaseSet[Int]]
class IntChecks             extends LatticePropertyChecks[Int]
class SetChecks             extends LatticePropertyChecks[Set[String]]
class MapChecks             extends LatticePropertyChecks[Map[String, Int]]
class OptionChecks          extends LatticePropertyChecks[Option[Int]]
class CusalLwwChecks        extends LatticePropertyChecks[LastWriterWins[Int]]
class LWWOptionChecks       extends LatticePropertyChecks[Option[LastWriterWins[Int]]]
class MultiValueChecks      extends LatticePropertyChecks[MultiValueRegister[Int]]
class OrSetChecks           extends LatticePropertyChecks[ObserveRemoveSet[Int]]
class PosNegChecks          extends LatticePropertyChecks[PosNegCounter]
class TupleChecks           extends LatticePropertyChecks[(Set[Int], GrowOnlyCounter)]
class VectorClockChecks     extends LatticePropertyChecks[VectorClock]
class GrowOnlyListChecks    extends LatticePropertyChecks[GrowOnlyList[Int]]
class LWWTupleChecks
    extends LatticePropertyChecks[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]

inline given bottomOption[A]: Option[Bottom[A]] =
  scala.compiletime.summonFrom:
    case b: Bottom[A] => Some(b)
    case _            => None

abstract class LatticePropertyChecks[A: Arbitrary: Lattice](using bot: Option[Bottom[A]])
    extends OrderTests(total = false)(using Lattice.latticeOrder, summon) {

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
        assertEquals(abc, abc2, s"merge not equal, steps:\n  $ab\n  $bc")
    }
  }

  val empty = bot.map(_.empty)

  property("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed

      val isDotted = theValue.isInstanceOf[Dotted[_]]

      decomposed.foreach { d =>
        // assertEquals(d merge theValue, Lattice.normalize(theValue), "naive order broken")
        assert(Lattice[A].lteq(d, theValue), s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d merge theValue}")
        empty match
          case Some(empty) => assertNotEquals(empty, d, "decomposed result was empty")
          case other       =>
        if isDotted
        then
          // do some extra checks which will cause failure later, but have better error reporting when done here
          decomposed.foreach: other =>
            if d != other
            then
              val thisCtx  = d.asInstanceOf[Dotted[_]].context
              val otherCtx = other.asInstanceOf[Dotted[_]].context
              assert(thisCtx disjunct otherCtx, s"overlapping context\n  ${d}\n  ${other}")
      }

      empty match
        case Some(empty) => assertEquals(empty merge theValue, Lattice.normalize(theValue), "bottom is bottom")
        case other       =>

      val merged = decomposed.reduceLeftOption(Lattice.merge)

      assertEquals(merged.orElse(empty), Some(Lattice.normalize(theValue)), s"decompose does not recompose")

    }
  }

  property("merge agrees with order"):
    forAll: (left: A, right: A) =>
      val merged = left merge right
      assert(left <= merged)
      assert(right <= merged)

}

