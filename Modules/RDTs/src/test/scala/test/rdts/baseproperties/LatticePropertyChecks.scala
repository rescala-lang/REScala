package test.rdts.baseproperties

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen, Shrink}
import rdts.base.{Bottom, BottomOpt, Lattice}
import rdts.datatypes.alternatives.{MultiValueRegister, ObserveRemoveSet}
import rdts.datatypes.contextual.{CausalQueue, ReplicatedList}
import rdts.datatypes.experiments.AutomergyOpGraphLWW.OpGraph
import rdts.datatypes.experiments.CausalStore
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyList, GrowOnlyMap, LastWriterWins, PosNegCounter, TwoPhaseSet, contextual}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots, Time, VectorClock}
import test.rdts.DataGenerator.RGAGen.given
import test.rdts.DataGenerator.{*, given}
import test.rdts.isGithubCi

// TODO, or maybe a note:
// These tests potentially fail if certain assumptions of the data types are invalidated by the generation strategy.
// Specifically, some values assume uniqueness, but the data generators don’t ensure uniqueness.
// Most notably, the Multi-Version-Register sometimes generates unique IDs associated with two different values, merging those is no longer commutative.
// This currently happens rarely enough, that a fix is postponed until a better strategy in general is found (just not allowing ID reuse might work, but would also exclude possible correct states, reducing the chance to find bugs. Though, that does not seem to be very high anyway …)

class OpGraphChecks           extends LatticePropertyChecks[OpGraph[ExampleData]]
class CausalStoreChecks       extends LatticePropertyChecks[CausalStore[Map[Dot, ExampleData]]]
class DottedCausalStoreChecks extends LatticePropertyChecks[Dotted[CausalStore[Map[Dot, ExampleData]]]]
class CausalQueueChecks       extends LatticePropertyChecks[Dotted[CausalQueue[ExampleData]]]
class DotSetChecks            extends LatticePropertyChecks[Dotted[Dots]]
class EnableWinsFlagChecks    extends LatticePropertyChecks[Dotted[contextual.EnableWinsFlag]]
class DotFunChecks            extends LatticePropertyChecks[Dotted[Map[Dot, Int]]]
class DotFunExampleChecks     extends LatticePropertyChecks[Dotted[Map[Dot, ExampleData]]]
class ConMultiVersionChecks   extends LatticePropertyChecks[Dotted[contextual.MultiVersionRegister[Int]]]
class DotMapChecks            extends LatticePropertyChecks[Dotted[Map[rdts.base.Uid, Dots]]](expensive = true)
class GrowOnlyCounterChecks   extends LatticePropertyChecks[GrowOnlyCounter]
class GrowOnlyMapChecks       extends LatticePropertyChecks[GrowOnlyMap[String, Int]]
class TwoPhaseSetChecks       extends LatticePropertyChecks[TwoPhaseSet[Int]]
class IntChecks               extends LatticePropertyChecks[Int]
class SetChecks               extends LatticePropertyChecks[Set[String]]
class MapChecks               extends LatticePropertyChecks[Map[String, Int]]
class OptionChecks            extends LatticePropertyChecks[Option[Int]]
class CusalLwwChecks          extends LatticePropertyChecks[LastWriterWins[Int]]
class LWWOptionChecks         extends LatticePropertyChecks[Option[LastWriterWins[Int]]]
class MultiValueChecks        extends LatticePropertyChecks[MultiValueRegister[Int]]
class OrSetChecks             extends LatticePropertyChecks[ObserveRemoveSet[Int]]
class PosNegChecks            extends LatticePropertyChecks[PosNegCounter]
class TupleChecks             extends LatticePropertyChecks[(Set[Int], GrowOnlyCounter)]
class VectorClockChecks       extends LatticePropertyChecks[VectorClock]
class GrowOnlyListChecks      extends LatticePropertyChecks[GrowOnlyList[Int]](expensive = true)
class ReplicatedListChecks    extends LatticePropertyChecks[Dotted[ReplicatedList[ExampleData]]](expensive = true)
class LWWTupleChecks
    extends LatticePropertyChecks[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]

abstract class LatticePropertyChecks[A](expensive: Boolean = false)(using
    arbitrary: Arbitrary[A],
    lattice: Lattice[A],
    bottomOpt: BottomOpt[A],
    shrink: Shrink[A]
) extends OrderTests(using Lattice.latticeOrder)(total = false) {

  override def munitIgnore: Boolean = expensive && isGithubCi

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
      val ab_c = Lattice.merge(ab, c)
      val a_bc = Lattice.merge(a, bc)
      assertEquals(ab_c, a_bc, s"merge not equal, steps:\n  $ab\n  $bc")

      val bc_ab = bc merge ab
      assertEquals(bc_ab, ab_c, "variation on idempotent & commutative to work around insufficient test generators")
    }
  }

  property("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed
      val normalized = Lattice.normalize(theValue)

      val isDotted = theValue.isInstanceOf[Dotted[?]]

      decomposed.foreach { d =>
        assertEquals(
          d merge theValue,
          normalized,
          s"naive order broken:\n ${d}\n $theValue\n${decomposed.mkString("   ", "\n   ", "\n")}"
        )
        assert(Lattice[A].lteq(d, theValue), s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d merge theValue}")
        if decomposed.sizeIs > 1
        then
          BottomOpt.explicit: bo =>
            assertNotEquals(bo.empty, d, s"decomposed result was empty\n  $decomposed")
        if isDotted
        then
          // do some extra checks which will cause failure later, but have better error reporting when done here
          decomposed.foreach: other =>
            if d != other
            then
              val thisCtx  = d.asInstanceOf[Dotted[?]].context
              val otherCtx = other.asInstanceOf[Dotted[?]].context
              assert(thisCtx disjunct otherCtx, s"overlapping context\n  ${d}\n  ${other}")
      }

      BottomOpt.explicit: bo =>
        assertEquals(bo.empty merge theValue, normalized, "bottom is bottom")

      val merged =
        if decomposed.sizeIs == 1
        then Some(Lattice.normalize(decomposed.head))
        else decomposed.reduceLeftOption(Lattice.merge)

      assertEquals(
        merged.orElse(BottomOpt.explicit(_.empty)),
        Some(normalized),
        s"decompose does not recompose (test may require a bottom instance if any component decomposes into None)"
      )

    }
  }

  property("merge agrees with order"):
    forAll: (left: A, right: A) =>
      val merged = left merge right

      assertEquals(left merge merged, merged, "naive lteq")
      assertEquals(right merge merged, merged, "naive lteq")
      assert(left <= merged, s"merged:\n  ${merged}\n ${left merge merged}")
      assert(right <= merged, s"merged:\n  ${merged}\n ${right merge merged}")
      assert(!(merged <= left) || merged == Lattice.normalize(left), s"merged:\n  ${merged}")
      assert(!(merged <= right) || merged == Lattice.normalize(right), s"merged:\n  ${merged}")

}
