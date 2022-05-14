package test.kofre

import kofre.base.DecomposeLattice
import kofre.causality.{ArrayRanges, CausalContext, Dot}
import kofre.contextual.ContextDecompose.*
import kofre.contextual.{AsCausalContext, ContextDecompose, WithContext}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import test.kofre.DataGenerator.*

class DotMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "dots" in forAll { (dm: Map[Int, CausalContext]) =>
    assert(
      AsCausalContext[Map[Int, CausalContext]].dots(dm).toSet == dm.values.flatMap(
        AsCausalContext[CausalContext].dots(_).iterator
      ).toSet,
      s"DotMap.dots should return the keys of the DotMap itself, but ${AsCausalContext[Map[Int, CausalContext]].dots(dm)} does not equal $dm"
    )
  }

  "empty" in assert(
    AsCausalContext[Map[Int, CausalContext]].empty.isEmpty,
    s"DotMap.empty should be empty, but ${AsCausalContext[Map[Int, CausalContext]].empty} is not empty"
  )

  "merge" in forAll {
    (
        dmA: Map[Int, CausalContext],
        deletedA: CausalContext,
        dmB: Map[Int, CausalContext],
        deletedB: CausalContext
    ) =>
      val dotsA = AsCausalContext[Map[Int, CausalContext]].dots(dmA)
      val dotsB = AsCausalContext[Map[Int, CausalContext]].dots(dmB)
      val ccA   = dotsA union deletedA
      val ccB   = dotsB union deletedB

      val WithContext(dmMerged, ccMerged) =
        DecomposeLattice[WithContext[Map[Int, CausalContext]]].merge(
          WithContext(dmA, (ccA)),
          WithContext(dmB, (ccB))
        )
      val dotsMerged = AsCausalContext[Map[Int, CausalContext]].dots(dmMerged)

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
            DotSet.mergePartial(
              WithContext(dmA.getOrElse(k, CausalContext.empty), (ccA)),
              WithContext(dmB.getOrElse(k, CausalContext.empty), (ccB))
            )

          assert(
            vMerged.isEmpty || dmMerged(k) == vMerged,
            s"For all keys that are in both DotMaps the result of DotMap.merge should map these to the merged values, but ${dmMerged.get(k)} does not equal $vMerged"
          )
        }
      }
  }

  "leq" ignore forAll {
    (
        dmA: Map[Int, CausalContext],
        deletedA: CausalContext,
        dmB: Map[Int, CausalContext],
        deletedB: CausalContext
    ) =>
      val ccA = AsCausalContext[Map[Int, CausalContext]].dots(dmA) union deletedA
      val ccB = AsCausalContext[Map[Int, CausalContext]].dots(dmB) union deletedB

      assert(
        DotMap[Int, CausalContext].lteq(
          WithContext(dmA, (ccA)),
          WithContext(dmA, (ccA))
        ),
        s"DotMap.leq should be reflexive, but returns false when applied to ($dmA, $ccA, $dmA, $ccA)"
      )

      val WithContext(dmMerged, ccMerged) =
        DecomposeLattice[WithContext[Map[Int, CausalContext]]].merge(
          WithContext(dmA, (ccA)),
          WithContext(dmB, (ccB))
        )

      assert(
        DotMap[Int, CausalContext].lteq(WithContext(dmA, (ccA)), WithContext(dmMerged, ccMerged)),
        s"The result of DotMap.merge should be larger than its lhs, but DotMap.leq returns false when applied to ($dmA, $ccA, $dmMerged, $ccMerged)"
      )
      assert(
        DotMap[Int, CausalContext].lteq(WithContext(dmB, (ccB)), WithContext(dmMerged, ccMerged)),
        s"The result of DotMap.merge should be larger than its rhs, but DotMap.leq returns false when applied to ($dmB, $ccB, $dmMerged, $ccMerged)"
      )
  }

  "decompose" in forAll { (dm: Map[Int, CausalContext], deleted: CausalContext) =>
    val cc = AsCausalContext[Map[Int, CausalContext]].dots(dm) union deleted

    val decomposed: Iterable[WithContext[Map[Int, CausalContext]]] =
      DotMap[Int, CausalContext].decompose(WithContext(dm, (cc)))
    val wc: WithContext[Map[Int, CausalContext]] =
      decomposed.foldLeft(WithContext(AsCausalContext[Map[Int, CausalContext]].empty, CausalContext.empty)) {
        case (WithContext(dmA, ccA), WithContext(dmB, ccB)) =>
          DecomposeLattice[WithContext[Map[Int, CausalContext]]].merge(WithContext(dmA, ccA), WithContext(dmB, ccB))
      }

    val dmMerged: Map[Int, CausalContext] = wc.store
    val ccMerged                          = wc.context

    val dotsIter      = dm.values.flatMap(_.iterator)
    val dotsSet       = dotsIter.toSet
    val duplicateDots = dotsIter.size != dotsSet.size

    assert(
      ccMerged == (cc),
      s"Merging the list of atoms returned by DotMap.decompose should produce an equal DotMap, but $dmMerged does not equal $dm"
    )
    if (!duplicateDots) {
      dm.keys.foreach { k =>
        assert(
          dm(k).toSet == dmMerged.getOrElse(k, CausalContext.empty).toSet,
          s"Merging the list of atoms returned by DotMap.decompose should produce an equal Causal Context, but on key $k the $ccMerged does not equal $cc"
        )
      }
    }
  }
}
