package test.rdts.corestructs

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.{Decompose, Lattice}
import rdts.dotted.Dotted
import rdts.dotted.HasDots.mapInstance
import rdts.time.{ArrayRanges, Dot, Dots}
import test.rdts.DataGenerator.{*, given}
import Lattice.syntax

import scala.annotation.tailrec

class DotMapTest extends munit.ScalaCheckSuite {

  type TestedMap = Map[Int, Dots]

  property("dots") {
    forAll { (dm: TestedMap) =>
      assertEquals(
        dm.dots.toSet,
        clue(dm).values.flatMap(
          _.dots.iterator
        ).toSet,
        s"DotMap.dots should return the keys of the DotMap itself,"
      )
    }
  }
  test("empty") {
    assert(
      Map.empty.isEmpty,
      s"DotMap.empty should be empty, but ${Map.empty} is not empty"
    )

  }
  property("merge") {
    forAll {
      (
          dmA: TestedMap,
          deletedA: Dots,
          dmB: TestedMap,
          deletedB: Dots
      ) =>
        val dotsA = dmA.dots
        val dotsB = dmB.dots
        val ccA   = dotsA `union` deletedA
        val ccB   = dotsB `union` deletedB

        val Dotted(dmMerged, ccMerged) =
          Lattice.merge(
            Dotted(dmA, (ccA)),
            Dotted(dmB, (ccB))
          )
        val dotsMerged = dmMerged.dots

        assert(
          ccMerged == (ccA `union` ccB),
          s"DotMap.merge should have the same effect as set `union` on the causal context, but $ccMerged does not equal ${ccA `union` ccB}"
        )
        assert(
          dotsMerged.toSet subsetOf (dotsA `union` dotsB).toSet,
          s"DotMap.merge should not add new elements to the DotSet, but $dotsMerged is not a subset of ${dotsA `union` dotsB}"
        )
        assert(
          (dotsMerged `intersect` (deletedA `diff` dotsA)).isEmpty,
          s"The DotMap resulting from DotMap.merge should not contain dots that were deleted on the lhs, but $dotsMerged contains elements from ${deletedA `diff` dotsA}"
        )
        assert(
          (dotsMerged `intersect` (deletedB `diff` dotsB)).isEmpty,
          s"The DotMap resulting from DotMap.merge should not contain dots that were deleted on the rhs, but $dotsMerged contains elements from ${deletedB `diff` dotsB}"
        )

        // ignore cases where the dots intersect, as this check does not seem to handle such cases correcly
        if dotsA.intersect(dotsB).isEmpty then {
          (dmA.keySet `union` dmB.keySet).foreach { k =>
            val vMerged =
              Dotted(dmA.getOrElse(k, Dots.empty), (ccA)) `merge` Dotted(dmB.getOrElse(k, Dots.empty), (ccB))

            assert(
              vMerged.data.isEmpty || dmMerged(k) == vMerged.data,
              s"For all keys that are in both DotMaps the result of DotMap.merge should map these to the merged values, but ${dmMerged.get(k)} does not equal $vMerged"
            )
          }
        }
    }
  }

  property("leq") {
    forAll {
      (
          dmA: TestedMap,
          deletedA: Dots,
          dmB: TestedMap,
          deletedB: Dots
      ) =>
        val ccA = dmA.dots `union` deletedA
        val ccB = dmB.dots `union` deletedB

        val dottedA = Dotted(dmA, ccA)
        val dottedB = Dotted(dmB, ccB)

        assert(
          Dotted(dmA, (ccA)).subsumes(dottedA),
          s"DotMap.leq should be reflexive, but returns false when applied to ($dmA, $ccA)"
        )

        val merged =
          Lattice.merge(
            dottedA,
            dottedB
          )

        assert(
          merged.subsumes(dottedA),
          s"The result of DotMap.merge should be larger than its lhs, but DotMap.leq returns false when applied to:\n  $dottedA\n  $merged"
        )
        assert(
          merged.subsumes(dottedB),
          s"The result of DotMap.merge should be larger than its rhs, but DotMap.leq returns false when applied to\n  $dottedB\n  $merged"
        )
    }

  }

  @tailrec
  private def removeDuplicates(
      start: List[(Int, Dots)],
      acc: TestedMap,
      con: Dots
  ): TestedMap =
    start match
      case Nil         => acc
      case (i, c) :: t => removeDuplicates(t, acc.updated(i, c.subtract(con)), con `union` c.dots)

  property("decompose") {
    forAll { (dmdup: TestedMap, deleted: Dots) =>

      val dm: TestedMap = removeDuplicates(dmdup.toList, Map.empty, Dots.empty)

      val cc = dm.dots `union` deleted

      val decomposed: Iterable[Dotted[TestedMap]] =
        Decompose.decompose(Dotted(dm, (cc)))
      val wc: Dotted[TestedMap] =
        decomposed.foldLeft(Dotted(Map.empty[Int, Dots], Dots.empty)) {
          case (Dotted(dmA, ccA), Dotted(dmB, ccB)) =>
            Lattice.merge(Dotted(dmA, ccA), Dotted(dmB, ccB))
        }

      val dmMerged: TestedMap = wc.data
      val ccMerged            = wc.context

      assertEquals(
        ccMerged,
        cc,
        s"Merging the list of atoms returned by DotMap.decompose should produce an equal DotMap, but $dmMerged does not equal $dm"
      )
      dm.keys.foreach { k =>
        assertEquals(
          dm(k),
          dmMerged.getOrElse(k, Dots.empty),
          s"Merging the list of atoms returned by DotMap.decompose should produce an equal Causal Context, but on key $k the $ccMerged does not equal $cc"
        )
      }
    }
  }
}
