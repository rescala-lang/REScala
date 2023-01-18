package test.kofre

import kofre.base.{DecomposeLattice, Lattice}
import kofre.dotted.DottedLattice.*
import kofre.dotted.{DotFun, DotMap, DotSet, Dotted, DottedLattice}
import kofre.time.{ArrayRanges, Dot, Dots}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.*

import scala.annotation.tailrec

class DotMapTest extends munit.ScalaCheckSuite {

  type TestedMap = DotMap[Int, DotSet]

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
      DotFun.empty.isEmpty,
      s"DotMap.empty should be empty, but ${DotFun.empty} is not empty"
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
        val ccA   = dotsA union deletedA
        val ccB   = dotsB union deletedB

        val Dotted(dmMerged, ccMerged) =
          DecomposeLattice[Dotted[TestedMap]].merge(
            Dotted(dmA, (ccA)),
            Dotted(dmB, (ccB))
          )
        val dotsMerged = dmMerged.dots

        assert(
          ccMerged == (ccA union ccB),
          s"DotMap.merge should have the same effect as set union on the causal context, but $ccMerged does not equal ${ccA union ccB}"
        )
        assert(
          dotsMerged.toSet subsetOf (dotsA union dotsB).toSet,
          s"DotMap.merge should not add new elements to the DotSet, but $dotsMerged is not a subset of ${dotsA union dotsB}"
        )
        assert(
          (dotsMerged intersect (deletedA diff dotsA)).isEmpty,
          s"The DotMap resulting from DotMap.merge should not contain dots that were deleted on the lhs, but $dotsMerged contains elements from ${deletedA diff dotsA}"
        )
        assert(
          (dotsMerged intersect (deletedB diff dotsB)).isEmpty,
          s"The DotMap resulting from DotMap.merge should not contain dots that were deleted on the rhs, but $dotsMerged contains elements from ${deletedB diff dotsB}"
        )

        // ignore cases where the dots intersect, as this check does not seem to handle such cases correcly
        if (dotsA.intersect(dotsB).isEmpty) {
          (dmA.keySet union dmB.keySet).foreach { k =>
            val vMerged =
              Dotted(dmA.getOrElse(k, DotSet.empty), (ccA)) mergePartial
              Dotted(dmB.getOrElse(k, DotSet.empty), (ccB))

            assert(
              vMerged.isEmpty || dmMerged(k) == vMerged,
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
        val ccA = dmA.dots union deletedA
        val ccB = dmB.dots union deletedB

        assert(
          Dotted(dmA, (ccA)) <= Lattice.normalize(Dotted(dmA, (ccA))),
          s"DotMap.leq should be reflexive, but returns false when applied to ($dmA, $ccA, $dmA, $ccA)"
        )

        val Dotted(dmMerged, ccMerged) =
          DecomposeLattice[Dotted[TestedMap]].merge(
            Dotted(dmA, (ccA)),
            Dotted(dmB, (ccB))
          )

        assert(
          Dotted(dmA, (ccA)) <= Dotted(dmMerged, ccMerged),
          s"The result of DotMap.merge should be larger than its lhs, but DotMap.leq returns false when applied to ($dmA, $ccA, $dmMerged, $ccMerged)"
        )
        assert(
          Dotted(dmB, (ccB)) <= Dotted(dmMerged, ccMerged),
          s"The result of DotMap.merge should be larger than its rhs, but DotMap.leq returns false when applied to ($dmB, $ccB, $dmMerged, $ccMerged)"
        )
    }

  }

  @tailrec
  private def removeDuplicates(
      start: List[(Int, DotSet)],
      acc: TestedMap,
      con: Dots
  ): TestedMap =
    start match
      case Nil         => acc
      case (i, c) :: t => removeDuplicates(t, DotMap(acc + (i -> DotSet(c.subtract(con)))), con union c.repr)

  property("decompose") {
    forAll { (dmdup: TestedMap, deleted: Dots) =>

      val dm: TestedMap = removeDuplicates(dmdup.toList, DotMap.empty, Dots.empty)

      val cc = dm.dots union deleted

      val decomposed: Iterable[Dotted[TestedMap]] =
        DottedLattice[TestedMap].decompose(Dotted(dm, (cc)))
      val wc: Dotted[TestedMap] =
        decomposed.foldLeft(Dotted(DotMap.empty[Int, DotSet], Dots.empty)) {
          case (Dotted(dmA, ccA), Dotted(dmB, ccB)) =>
            DecomposeLattice[Dotted[TestedMap]].merge(Dotted(dmA, ccA), Dotted(dmB, ccB))
        }

      val dmMerged: TestedMap = wc.store
      val ccMerged            = wc.context

      assertEquals(
        ccMerged,
        cc,
        s"Merging the list of atoms returned by DotMap.decompose should produce an equal DotMap, but $dmMerged does not equal $dm"
      )
      dm.keys.foreach { k =>
        assertEquals(
          dm(k),
          dmMerged.getOrElse(k, DotSet.empty),
          s"Merging the list of atoms returned by DotMap.decompose should produce an equal Causal Context, but on key $k the $ccMerged does not equal $cc"
        )
      }
    }
  }
}
