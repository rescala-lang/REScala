package test.kofre.baseproperties

import kofre.base.{Bottom, BottomOpt, Lattice}
import kofre.datatypes.alternatives.{MultiValueRegister, ObserveRemoveSet}
import kofre.datatypes.contextual.{CausalQueue, ReplicatedList}
import kofre.datatypes.experiments.AutomergyOpGraphLWW.OpGraph
import kofre.datatypes.experiments.CausalStore
import kofre.datatypes.{GrowOnlyCounter, GrowOnlyList, GrowOnlyMap, LastWriterWins, PosNegCounter, TwoPhaseSet, contextual}
import kofre.dotted.{DotFun, DotMap, DotSet, Dotted, HasDots}
import kofre.time.{Dots, Time, VectorClock}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen, Shrink}
import test.kofre.DataGenerator.{*, given}
import test.kofre.DataGenerator.RGAGen.given
import test.kofre.isGithubCi

class OpGraphChecks           extends LatticePropertyChecks[OpGraph[ExampleData]]
class CausalStoreChecks       extends LatticePropertyChecks[CausalStore[DotFun[ExampleData]]]
class DottedCausalStoreChecks extends LatticePropertyChecks[Dotted[CausalStore[DotFun[ExampleData]]]]
class CausalQueueChecks       extends LatticePropertyChecks[Dotted[CausalQueue[ExampleData]]]
class DotSetChecks            extends LatticePropertyChecks[Dotted[DotSet]]
class EnableWinsFlagChecks    extends LatticePropertyChecks[Dotted[contextual.EnableWinsFlag]]
class DotFunChecks            extends LatticePropertyChecks[Dotted[DotFun[Int]]]
class DotFunExampleChecks     extends LatticePropertyChecks[Dotted[DotFun[ExampleData]]]
class ConMultiVersionChecks   extends LatticePropertyChecks[Dotted[contextual.MultiVersionRegister[Int]]]
class DotMapChecks            extends LatticePropertyChecks[Dotted[DotMap[kofre.base.Uid, DotSet]]](expensive = true)
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

abstract class LatticePropertyChecks[A](expensive: Boolean = false)(using arbitrary: Arbitrary[A], lattice: Lattice[A], bottomOpt: BottomOpt[A], shrink: Shrink[A])
    extends OrderTests(using Lattice.latticeOrder)(total = false) {

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

      val isDotted = theValue.isInstanceOf[Dotted[_]]

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
              val thisCtx  = d.asInstanceOf[Dotted[_]].context
              val otherCtx = other.asInstanceOf[Dotted[_]].context
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
