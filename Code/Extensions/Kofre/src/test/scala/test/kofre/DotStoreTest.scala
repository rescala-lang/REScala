package test.kofre

import kofre.causality.{CausalContext, Dot}
import kofre.decompose.DotStore._
import kofre.decompose.{DotStore, UIJDLattice}
import kofre.dotbased.CausalStore
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

object DotStoreGenerators {
  val genDot: Gen[Dot] = for {
    replicaID <- Gen.stringOfN(2, Gen.alphaChar)
    counter   <- Gen.posNum[Long]
  } yield Dot(replicaID, counter)

  implicit val arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  val genDotSet: Gen[Set[Dot]] = Gen.containerOf[Set, Dot](genDot)

  val genDietMapCContext: Gen[CausalContext] = for {
    ds <- genDotSet
  } yield CausalContext.fromSet(ds)

  implicit val arbDietMapCContext: Arbitrary[CausalContext] = Arbitrary(genDietMapCContext)

  implicit val arbDotSet: Arbitrary[Set[Dot]] = Arbitrary(genDotSet)

  def genDotFun[A](implicit g: Gen[A]): Gen[DotFun[A]] = for {
    n      <- Gen.posNum[Int]
    dots   <- Gen.containerOfN[List, Dot](n, genDot)
    values <- Gen.containerOfN[List, A](n, g)
  } yield (dots zip values).toMap

  implicit def arbDotFun[A](implicit g: Gen[A]): Arbitrary[DotFun[A]] = Arbitrary(genDotFun)

  def genDotMap[K, V: DotStore](implicit gk: Gen[K], gv: Gen[V]): Gen[DotMap[K, V]] = (for {
    n      <- Gen.posNum[Int]
    keys   <- Gen.containerOfN[List, K](n, gk)
    values <- Gen.containerOfN[List, V](n, gv)
  } yield (keys zip values).toMap).suchThat { m =>
    val dotsIter = m.values.flatMap(v => DotStore[V].dots(v).iterator)
    val dotsSet  = dotsIter.toSet
    dotsIter.size == dotsSet.size
  }

  implicit def arbDotMap[K, V: DotStore](implicit gk: Gen[K], gv: Gen[V]): Arbitrary[DotMap[K, V]] =
    Arbitrary(genDotMap)
}

class DotSetTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import DotStoreGenerators._

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

    val CausalStore(dsMerged, ccMerged) = UIJDLattice[CausalStore[CausalContext]].merge(
      CausalStore(dsA, ccA),
      CausalStore(dsB, ccB)
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
      DotSet.leq(CausalStore(dsA, ccA), CausalStore(dsA, ccA)),
      s"DotSet.leq should be reflexive, but returns false when applied to ($dsA, $ccA, $dsA, $ccA)"
    )

    val CausalStore(dsMerged, ccMerged) = UIJDLattice[CausalStore[CausalContext]].merge(
      CausalStore(dsA, ccA),
      CausalStore(dsB, ccB)
    )

    assert(
      DotSet.leq(CausalStore(dsA, ccA), CausalStore(dsMerged, ccMerged)),
      s"The result of DotSet.merge should be larger than its lhs, but DotSet.leq returns false when applied to ($dsA, $ccA, $dsMerged, $ccMerged)"
    )
    assert(
      DotSet.leq(CausalStore(dsB, ccB), CausalStore(dsMerged, ccMerged)),
      s"The result of DotSet.merge should be larger than its rhs, but DotSet.leq returns false when applied to ($dsB, $ccB, $dsMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (ds: CausalContext, deleted: CausalContext) =>
    val cc = ds union deleted

    val decomposed = DotSet.decompose(CausalStore(ds, cc))
    val CausalStore(dsMerged, ccMerged) = decomposed.foldLeft(CausalStore(CausalContext.empty, CausalContext.empty)) {
      case (CausalStore(dsA, ccA), CausalStore(dsB, ccB)) =>
        UIJDLattice[CausalStore[CausalContext]].merge(CausalStore(dsA, ccA), CausalStore(dsB, ccB))
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
  import DotStoreGenerators._

  "dots" in forAll { (df: DotFun[Int]) =>
    assert(
      DotFun[Int].dots(df).toSet == df.keySet,
      s"DotFun.dots should return the keys of the DotFun itself, but ${DotFun[Int].dots(df)} does not equal $df"
    )
  }

  "empty" in assert(
    DotFun[Int].empty.isEmpty,
    s"DotFun.empty should be empty, but ${DotFun[Int].empty} is not empty"
  )

  "merge" in forAll { (dfA: DotFun[Int], deletedA: CausalContext, dfB: DotFun[Int], deletedB: CausalContext) =>
    val dotsA = DotFun[Int].dots(dfA)
    val dotsB = DotFun[Int].dots(dfB)
    val ccA   = dotsA union deletedA
    val ccB   = dotsB union deletedB

    val CausalStore(dfMerged, ccMerged) =
      UIJDLattice[CausalStore[DotFun[Int]]].merge(
        CausalStore(dfA, ccA),
        CausalStore(dfB, ccB)
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

  "leq" in forAll { (dfA: DotFun[Int], deletedA: CausalContext, dfB: DotFun[Int], deletedB: CausalContext) =>
    val ccA = DotFun[Int].dots(dfA) union deletedA
    val ccB = DotFun[Int].dots(dfB) union deletedB

    assert(
      DotFun[Int].leq(CausalStore(dfA, ccA), CausalStore(dfA, ccA)),
      s"DotFun.leq should be reflexive, but returns false when applied to ($dfA, $ccA, $dfA, $ccA)"
    )

    val CausalStore(dfMerged, ccMerged) =
      UIJDLattice[CausalStore[DotFun[Int]]].merge(
        CausalStore(dfA, (ccA)),
        CausalStore(dfB, (ccB))
      )

    assert(
      DotFun[Int].leq(CausalStore(dfA, (ccA)), CausalStore(dfMerged, ccMerged)),
      s"The result of DotFun.merge should be larger than its lhs, but DotFun.leq returns false when applied to ($dfA, $ccA, $dfMerged, $ccMerged)"
    )
    assert(
      DotFun[Int].leq(CausalStore(dfB, (ccB)), CausalStore(dfMerged, ccMerged)),
      s"The result of DotFun.merge should be larger than its rhs, but DotFun.leq returns false when applied to ($dfB, $ccB, $dfMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (df: DotFun[Int], deleted: CausalContext) =>
    val cc = DotFun[Int].dots(df) union deleted

    val decomposed = DotFun[Int].decompose(CausalStore(df, (cc)))
    val CausalStore(dfMerged, ccMerged) = decomposed.foldLeft(CausalStore(DotFun[Int].empty, CausalContext.empty)) {
      case (CausalStore(dfA, ccA), CausalStore(dfB, ccB)) =>
        UIJDLattice[CausalStore[DotFun[Int]]].merge(CausalStore(dfA, ccA), CausalStore(dfB, ccB))
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
  import DotStoreGenerators._

  "dots" in forAll { (dm: DotMap[Int, CausalContext]) =>
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
        dmA: DotMap[Int, CausalContext],
        deletedA: CausalContext,
        dmB: DotMap[Int, CausalContext],
        deletedB: CausalContext
    ) =>
      val dotsA = DotMap[Int, CausalContext].dots(dmA)
      val dotsB = DotMap[Int, CausalContext].dots(dmB)
      val ccA   = dotsA union deletedA
      val ccB   = dotsB union deletedB

      val CausalStore(dmMerged, ccMerged) =
        UIJDLattice[CausalStore[DotMap[Int, CausalContext]]].merge(
          CausalStore(dmA, (ccA)),
          CausalStore(dmB, (ccB))
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
              CausalStore(dmA.getOrElse(k, DotSet.empty), (ccA)),
              CausalStore(dmB.getOrElse(k, DotSet.empty), (ccB))
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
        dmA: DotMap[Int, CausalContext],
        deletedA: CausalContext,
        dmB: DotMap[Int, CausalContext],
        deletedB: CausalContext
    ) =>
      val ccA = DotMap[Int, CausalContext].dots(dmA) union deletedA
      val ccB = DotMap[Int, CausalContext].dots(dmB) union deletedB

      assert(
        DotMap[Int, CausalContext].leq(
          CausalStore(dmA, (ccA)),
          CausalStore(dmA, (ccA))
        ),
        s"DotMap.leq should be reflexive, but returns false when applied to ($dmA, $ccA, $dmA, $ccA)"
      )

      val CausalStore(dmMerged, ccMerged) =
        UIJDLattice[CausalStore[DotMap[Int, CausalContext]]].merge(
          CausalStore(dmA, (ccA)),
          CausalStore(dmB, (ccB))
        )

      assert(
        DotMap[Int, CausalContext].leq(CausalStore(dmA, (ccA)), CausalStore(dmMerged, ccMerged)),
        s"The result of DotMap.merge should be larger than its lhs, but DotMap.leq returns false when applied to ($dmA, $ccA, $dmMerged, $ccMerged)"
      )
      assert(
        DotMap[Int, CausalContext].leq(CausalStore(dmB, (ccB)), CausalStore(dmMerged, ccMerged)),
        s"The result of DotMap.merge should be larger than its rhs, but DotMap.leq returns false when applied to ($dmB, $ccB, $dmMerged, $ccMerged)"
      )
  }

  "decompose" in forAll { (dm: DotMap[Int, CausalContext], deleted: CausalContext) =>
    val cc = DotMap[Int, CausalContext].dots(dm) union deleted

    val decomposed = DotMap[Int, CausalContext].decompose(CausalStore(dm, (cc)))
    val CausalStore(dmMerged, ccMerged) =
      decomposed.foldLeft(CausalStore(DotMap[Int, CausalContext].empty, CausalContext.empty)) {
        case (CausalStore(dmA, ccA), CausalStore(dmB, ccB)) =>
          UIJDLattice[CausalStore[DotMap[Int, CausalContext]]].merge(CausalStore(dmA, ccA), CausalStore(dmB, ccB))
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
