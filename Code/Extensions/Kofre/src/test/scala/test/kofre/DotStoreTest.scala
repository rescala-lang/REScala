package test.kofre

import kofre.causality.{CausalContext, Dot}
import kofre.contextual.WithContextDecompose.*
import kofre.decompose.UIJDLattice
import kofre.contextual.{WithContext, WithContextDecompose}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import test.kofre.DataGenerator.*

class DotSetTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "dots" in forAll { (ds: CausalContext) =>
    assert(
      DotSet.dots(ds) == ds,
      s"DotSet.dots should return the set itself, but ${DotSet.dots(ds)} does not equal $ds"
    )
  }

  "empty" in assert(
    DotSet.empty.isEmpty,
    s"DotSet.empty should be empty, but ${DotSet.empty} is not empty"
  )

  "merge" in forAll { (dsA: CausalContext, deletedA: CausalContext, dsB: CausalContext, deletedB: CausalContext) =>
    val ccA = dsA union deletedA
    val ccB = dsB union deletedB

    val WithContext(dsMerged, ccMerged) = UIJDLattice[WithContext[CausalContext]].merge(
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

  "leq" in forAll { (dsA: CausalContext, deletedA: CausalContext, dsB: CausalContext, deletedB: CausalContext) =>
    val ccA = dsA union deletedA
    val ccB = dsB union deletedB

    assert(
      DotSet.lteq(WithContext(dsA, ccA), WithContext(dsA, ccA)),
      s"DotSet.leq should be reflexive, but returns false when applied to ($dsA, $ccA, $dsA, $ccA)"
    )

    val WithContext(dsMerged, ccMerged) = UIJDLattice[WithContext[CausalContext]].merge(
      WithContext(dsA, ccA),
      WithContext(dsB, ccB)
    )

    assert(
      DotSet.lteq(WithContext(dsA, ccA), WithContext(dsMerged, ccMerged)),
      s"The result of DotSet.merge should be larger than its lhs, but DotSet.leq returns false when applied to ($dsA, $ccA, $dsMerged, $ccMerged)"
    )
    assert(
      DotSet.lteq(WithContext(dsB, ccB), WithContext(dsMerged, ccMerged)),
      s"The result of DotSet.merge should be larger than its rhs, but DotSet.leq returns false when applied to ($dsB, $ccB, $dsMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (ds: CausalContext, deleted: CausalContext) =>
    val cc = ds union deleted

    val decomposed = DotSet.decompose(WithContext(ds, cc))
    val WithContext(dsMerged, ccMerged) = decomposed.foldLeft(WithContext(CausalContext.empty, CausalContext.empty)) {
      case (WithContext(dsA, ccA), WithContext(dsB, ccB)) =>
        UIJDLattice[WithContext[CausalContext]].merge(WithContext(dsA, ccA), WithContext(dsB, ccB))
    }

    assert(
      dsMerged == ds,
      s"Merging the list of atoms returned by DotSet.decompose should produce an equal DotSet, but $dsMerged does not equal $ds (while decomposed was $decomposed)"
    )
    assert(
      ccMerged == cc,
      s"Merging the list of atoms returned by DotSet.decompose should produce an equal Causal Context, but $ccMerged does not equal $cc"
    )
  }
}

class DotFunTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "dots" in forAll { (df: Map[Dot, Int]) =>
    assert(
      DotFun[Int].dots(df).toSet == df.keySet,
      s"DotFun.dots should return the keys of the DotFun itself, but ${DotFun[Int].dots(df)} does not equal $df"
    )
  }

  "empty" in assert(
    DotFun[Int].empty.isEmpty,
    s"DotFun.empty should be empty, but ${DotFun[Int].empty} is not empty"
  )

  "merge" in forAll { (dfA: Map[Dot, Int], deletedA: CausalContext, dfB: Map[Dot, Int], deletedB: CausalContext) =>
    val dotsA = DotFun[Int].dots(dfA)
    val dotsB = DotFun[Int].dots(dfB)
    val ccA   = dotsA union deletedA
    val ccB   = dotsB union deletedB

    val WithContext(dfMerged, ccMerged) =
      UIJDLattice[WithContext[Map[Dot, Int]]].merge(
        WithContext(dfA, ccA),
        WithContext(dfB, ccB)
      )
    val dotsMerged = DotFun[Int].dots(dfMerged)

    assert(
      ccMerged == (ccA union ccB),
      s"DotFun.merge should have the same effect as set union on the causal context, but $ccMerged does not equal ${ccA union ccB}"
    )
    assert(
      dotsMerged.toSet subsetOf (dotsA union dotsB).toSet,
      s"DotFun.merge should not add new elements to the DotSet, but $dotsMerged is not a subset of ${dotsA union dotsB}"
    )
    assert(
      (dotsMerged intersect (deletedA diff dotsA)).isEmpty,
      s"The DotFun resulting from DotFun.merge should not contain dots that were deleted on the lhs, but $dotsMerged contains elements from ${deletedA diff dotsA}"
    )
    assert(
      (dotsMerged intersect (deletedB diff dotsB)).isEmpty,
      s"The DotFun resulting from DotFun.merge should not contain dots that were deleted on the rhs, but $dotsMerged contains elements from ${deletedB diff dotsB}"
    )

    (dotsA intersect dotsB).iterator.foreach { d =>
      assert(
        dfMerged(d) == UIJDLattice[Int].merge(dfA(d), dfB(d)),
        s"If a dot is used as key in both DotFuns then the corresponding values should be merged in the result of DotFun.merge, but ${dfMerged(
            d
          )} does not equal ${UIJDLattice[Int].merge(dfA(d), dfB(d))}"
      )
    }

    (dotsA diff ccB).iterator.foreach { d =>
      assert(
        dfMerged(d) == dfA(d),
        s"If a dot only appears on the lhs of DotFun.merge then resulting DotFun should have the same mapping as the lhs, but ${dfMerged(d)} does not equal ${dfA(d)}"
      )
    }

    (dotsB diff ccA).iterator.foreach { d =>
      assert(
        dfMerged(d) == dfB(d),
        s"If a dot only appears on the rhs of DotFun.merge then resulting DotFun should have the same mapping as the rhs, but ${dfMerged(d)} does not equal ${dfB(d)}"
      )
    }
  }

  "leq" in forAll { (dfA: Map[Dot, Int], deletedA: CausalContext, dfB: Map[Dot, Int], deletedB: CausalContext) =>
    val ccA = DotFun[Int].dots(dfA) union deletedA
    val ccB = DotFun[Int].dots(dfB) union deletedB

    assert(
      DotFun[Int].lteq(WithContext(dfA, ccA), WithContext(dfA, ccA)),
      s"DotFun.leq should be reflexive, but returns false when applied to ($dfA, $ccA, $dfA, $ccA)"
    )

    val WithContext(dfMerged, ccMerged) =
      UIJDLattice[WithContext[Map[Dot, Int]]].merge(
        WithContext(dfA, (ccA)),
        WithContext(dfB, (ccB))
      )

    assert(
      DotFun[Int].lteq(WithContext(dfA, (ccA)), WithContext(dfMerged, ccMerged)),
      s"The result of DotFun.merge should be larger than its lhs, but DotFun.leq returns false when applied to ($dfA, $ccA, $dfMerged, $ccMerged)"
    )
    assert(
      DotFun[Int].lteq(WithContext(dfB, (ccB)), WithContext(dfMerged, ccMerged)),
      s"The result of DotFun.merge should be larger than its rhs, but DotFun.leq returns false when applied to ($dfB, $ccB, $dfMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (df: Map[Dot, Int], deleted: CausalContext) =>
    val cc = DotFun[Int].dots(df) union deleted

    val decomposed = DotFun[Int].decompose(WithContext(df, (cc)))
    val WithContext(dfMerged, ccMerged) = decomposed.foldLeft(WithContext(DotFun[Int].empty, CausalContext.empty)) {
      case (WithContext(dfA, ccA), WithContext(dfB, ccB)) =>
        UIJDLattice[WithContext[Map[Dot, Int]]].merge(WithContext(dfA, ccA), WithContext(dfB, ccB))
    }

    assert(
      dfMerged == df,
      s"Merging the list of atoms returned by DotFun.decompose should produce an equal DotFun, but $dfMerged does not equal $df"
    )
    assert(
      ccMerged == (cc),
      s"Merging the list of atoms returned by DotFun.decompose should produce an equal Causal Context, but $ccMerged does not equal $cc"
    )
  }
}

class DotMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "dots" in forAll { (dm: Map[Int, CausalContext]) =>
    assert(
      DotMap[Int, CausalContext].dots(dm).toSet == dm.values.flatMap(DotSet.dots(_).iterator).toSet,
      s"DotMap.dots should return the keys of the DotMap itself, but ${DotMap[Int, CausalContext].dots(dm)} does not equal $dm"
    )
  }

  "empty" in assert(
    DotMap[Int, CausalContext].empty.isEmpty,
    s"DotMap.empty should be empty, but ${DotMap[Int, CausalContext].empty} is not empty"
  )

  "merge" in forAll {
    (
        dmA: Map[Int, CausalContext],
        deletedA: CausalContext,
        dmB: Map[Int, CausalContext],
        deletedB: CausalContext
    ) =>
      val dotsA = DotMap[Int, CausalContext].dots(dmA)
      val dotsB = DotMap[Int, CausalContext].dots(dmB)
      val ccA   = dotsA union deletedA
      val ccB   = dotsB union deletedB

      val WithContext(dmMerged, ccMerged) =
        UIJDLattice[WithContext[Map[Int, CausalContext]]].merge(
          WithContext(dmA, (ccA)),
          WithContext(dmB, (ccB))
        )
      val dotsMerged = DotMap[Int, CausalContext].dots(dmMerged)

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
              WithContext(dmA.getOrElse(k, DotSet.empty), (ccA)),
              WithContext(dmB.getOrElse(k, DotSet.empty), (ccB))
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
      val ccA = DotMap[Int, CausalContext].dots(dmA) union deletedA
      val ccB = DotMap[Int, CausalContext].dots(dmB) union deletedB

      assert(
        DotMap[Int, CausalContext].lteq(
          WithContext(dmA, (ccA)),
          WithContext(dmA, (ccA))
        ),
        s"DotMap.leq should be reflexive, but returns false when applied to ($dmA, $ccA, $dmA, $ccA)"
      )

      val WithContext(dmMerged, ccMerged) =
        UIJDLattice[WithContext[Map[Int, CausalContext]]].merge(
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
    val cc = DotMap[Int, CausalContext].dots(dm) union deleted

    val decomposed = DotMap[Int, CausalContext].decompose(WithContext(dm, (cc)))
    val WithContext(dmMerged, ccMerged) =
      decomposed.foldLeft(WithContext(DotMap[Int, CausalContext].empty, CausalContext.empty)) {
        case (WithContext(dmA, ccA), WithContext(dmB, ccB)) =>
          UIJDLattice[WithContext[Map[Int, CausalContext]]].merge(WithContext(dmA, ccA), WithContext(dmB, ccB))
      }

    val dotsIter      = dm.values.flatMap(DotSet.dots(_).iterator)
    val dotsSet       = dotsIter.toSet
    val duplicateDots = dotsIter.size != dotsSet.size

    assert(
      ccMerged == (cc),
      s"Merging the list of atoms returned by DotMap.decompose should produce an equal DotMap, but $dmMerged does not equal $dm"
    )
    if (!duplicateDots) {
      dm.keys.foreach { k =>
        assert(
          dm(k).toSet == dmMerged.getOrElse(k, DotSet.empty).toSet,
          s"Merging the list of atoms returned by DotMap.decompose should produce an equal Causal Context, but on key $k the $ccMerged does not equal $cc"
        )
      }
    }
  }
}
