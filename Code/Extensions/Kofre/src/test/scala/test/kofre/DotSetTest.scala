package test.kofre

import kofre.base.DecomposeLattice
import kofre.causality.CausalContext
import kofre.contextual.{AsCausalContext, WithContext}
import kofre.dotted.DotSet
import org.scalacheck.Prop.*
import test.kofre.DataGenerator.*

class DotSetTest extends munit.ScalaCheckSuite {

  test("empty") {
    assert(
      DotSet.empty.isEmpty,
      s"DotSet.empty should be empty, but ${DotSet.empty} is not empty"
      )

  }
  property("merge") {
    forAll { (dsA: CausalContext, deletedA: CausalContext, dsB: CausalContext, deletedB: CausalContext) =>
      val ccA = dsA union deletedA
      val ccB = dsB union deletedB

      val WithContext(dsMerged, ccMerged) = DecomposeLattice[WithContext[CausalContext]].merge(
        WithContext(dsA, ccA),
        WithContext(dsB, ccB)
      )

      assert(
        ccMerged == (ccA union ccB),
        s"DotSet.merge should have the same effect as set union on the causal context, but $ccMerged does not equal ${ccA union ccB}"
      )
      assert(
        dsMerged.toSet subsetOf (dsA union dsB).toSet,
        s"DotSet.merge should not add new elements to the DotSet, but $dsMerged is not a subset of ${dsA union dsB}"
      )
      assert(
        (dsMerged intersect (deletedA diff dsA)).isEmpty,
        s"The DotSet resulting from DotSet.merge should not contain dots that were deleted on the lhs, but $dsMerged contains elements from ${deletedA diff dsA}"
      )
      assert(
        (dsMerged intersect (deletedB diff dsB)).isEmpty,
        s"The DotSet resulting from DotSet.merge should not contain dots that were deleted on the rhs, but $dsMerged contains elements from ${deletedB diff dsB}"
      )
    }

  }
  property("leq") {
    forAll { (dsA: CausalContext, deletedA: CausalContext, dsB: CausalContext, deletedB: CausalContext) =>
      val ccA = dsA union deletedA
      val ccB = dsB union deletedB

      assert(
        WithContext(dsA, ccA) <= WithContext(dsA, ccA),
        s"DotSet.leq should be reflexive, but returns false when applied to ($dsA, $ccA, $dsA, $ccA)"
      )

      val WithContext(dsMerged, ccMerged) = DecomposeLattice[WithContext[CausalContext]].merge(
        WithContext(dsA, ccA),
        WithContext(dsB, ccB)
      )

      assert(
        WithContext(dsA, ccA) <= WithContext(dsMerged, ccMerged),
        s"The result of DotSet.merge should be larger than its lhs, but DotSet.leq returns false when applied to ($dsA, $ccA, $dsMerged, $ccMerged)"
      )
      assert(
        WithContext(dsB, ccB) <= WithContext(dsMerged, ccMerged),
        s"The result of DotSet.merge should be larger than its rhs, but DotSet.leq returns false when applied to ($dsB, $ccB, $dsMerged, $ccMerged)"
      )
    }
  }

  property("decompose all") {
    forAll { (ds: CausalContext, deleted: CausalContext) =>
      val cc = ds union deleted

      val decomposed = WithContext(ds, cc).decomposed
      val WithContext(dsMerged, ccMerged) = decomposed.foldLeft(WithContext(CausalContext.empty, CausalContext.empty)) {
        case (WithContext(dsA, ccA), WithContext(dsB, ccB)) =>
          DecomposeLattice[WithContext[CausalContext]].merge(WithContext(dsA, ccA), WithContext(dsB, ccB))
      }

      assertEquals(
        dsMerged,
        ds,
        s"Merging the list of atoms returned by DotSet.decompose should produce an equal DotSet, but $dsMerged does not equal $ds (while decomposed was $decomposed)"
      )
      assertEquals(
        ccMerged,
        cc,
        s"Merging the list of atoms returned by DotSet.decompose should produce an equal Causal Context, but $ccMerged does not equal $cc"
      )
    }
  }
}
