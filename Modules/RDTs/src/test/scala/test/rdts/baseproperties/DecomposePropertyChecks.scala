package test.rdts.baseproperties

import munit.TestValues
import munit.internal.FutureCompat.ExtensionFuture
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Shrink}
import rdts.base.{Bottom, BottomOpt, Decompose, Lattice}
import rdts.datatypes.alternatives.MultiValueRegister
import rdts.datatypes.contextual.ReplicatedList
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyList, GrowOnlyMap, LastWriterWins, PosNegCounter, TwoPhaseSet, contextual}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}
import test.rdts.DataGenerator.RGAGen.given
import test.rdts.DataGenerator.{*, given}
import test.rdts.isGithubCi

import scala.util.{Failure, Success}

class DotSetDecomposeChecks          extends DecomposePropertyChecks[Dotted[Dots]]
class EnableWinsFlagDecomposeChecks  extends DecomposePropertyChecks[Dotted[contextual.EnableWinsFlag]]
class DotFunDecomposeChecks          extends DecomposePropertyChecks[Dotted[Map[Dot, Int]]]
class ConMultiVersionDecomposeChecks extends DecomposePropertyChecks[Dotted[contextual.MultiVersionRegister[Int]]]
class DotMapDecomposeChecks          extends DecomposePropertyChecks[Dotted[Map[rdts.base.Uid, Dots]]](expensive = true)
class GrowOnlyCounterDecomposeChecks extends DecomposePropertyChecks[GrowOnlyCounter]
class GrowOnlyMapDecomposeChecks     extends DecomposePropertyChecks[GrowOnlyMap[String, Int]]
class TwoPhaseSetDecomposeChecks     extends DecomposePropertyChecks[TwoPhaseSet[Int]]
class IntDecomposeChecks             extends DecomposePropertyChecks[Int]
class SetDecomposeChecks             extends DecomposePropertyChecks[Set[String]]
class MapDecomposeChecks             extends DecomposePropertyChecks[Map[String, Int]]
class MultiValueDecomposeChecks      extends DecomposePropertyChecks[MultiValueRegister[Int]](flaky = true)
class PosNegDecomposeChecks          extends DecomposePropertyChecks[PosNegCounter]
class TupleDecomposeChecks           extends DecomposePropertyChecks[(Set[Int], GrowOnlyCounter)]
class GrowOnlyListDecomposeChecks    extends DecomposePropertyChecks[GrowOnlyList[Int]](expensive = true)
class ReplicatedListDecomposeChecks
    extends DecomposePropertyChecks[Dotted[ReplicatedList[ExampleData]]](expensive = true)
class LWWTupleDecomposeChecks
    extends LatticePropertyChecks[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]

abstract class DecomposePropertyChecks[A](
    expensive: Boolean = false,
    flaky: Boolean = false
)(
    using
    arbitrary: Arbitrary[A],
    lattice: Lattice[A],
    decompose: Decompose[A],
    bottomOpt: BottomOpt[A],
    shrink: Shrink[A],
) extends munit.ScalaCheckSuite {

  override def munitIgnore: Boolean = expensive && isGithubCi

  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms ++ List(
    new TestTransform(
      "flakyTestGenarators",
      { t =>
        if !(flaky && isGithubCi) then t
        else
          t.withBodyMap(_.transformCompat {
            case Failure(exception) => Success(new TestValues.FlakyFailure(exception))
            case succ               => succ
          }(munitExecutionContext))
      }
    )
  )

  property("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed
      val normalized = Lattice.normalize(theValue)

      val isDotted = theValue.isInstanceOf[Dotted[?]]

      decomposed.foreach { d =>
        assertEquals(
          d `merge` theValue,
          normalized,
          s"naive order broken:\n ${d}\n $theValue\n${decomposed.mkString("   ", "\n   ", "\n")}"
        )
        assert(
          Lattice[A].subsumption(d, normalized),
          s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d `merge` normalized}"
        )
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
              assert(thisCtx `disjunct` otherCtx, s"overlapping context\n  ${d}\n  ${other}")
      }

      BottomOpt.explicit: bo =>
        assertEquals(bo.empty `merge` theValue, normalized, "bottom is bottom")

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
}
