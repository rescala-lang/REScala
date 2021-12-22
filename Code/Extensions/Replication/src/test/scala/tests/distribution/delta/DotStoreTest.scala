package tests.distribution.delta

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.CContext.SetCContext
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.{Causal, Dot, DotStore, UIJDLattice}
import tests.rescala.testtools.IgnoreOnGithubCiBecause

object DotStoreGenerators {
  val genDot: Gen[Dot] = for {
    replicaID <- Gen.alphaNumStr
    counter   <- Gen.posNum[Int]
  } yield Dot(replicaID, counter)

  implicit val arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  val genDotSet: Gen[DotSet] = Gen.containerOf[Set, Dot](genDot)

  implicit val arbDotSet: Arbitrary[DotSet] = Arbitrary(genDotSet)

  def genDotFun[A: UIJDLattice](implicit g: Gen[A]): Gen[DotFun[A]] = for {
    n      <- Gen.posNum[Int]
    dots   <- Gen.containerOfN[List, Dot](n, genDot)
    values <- Gen.containerOfN[List, A](n, g)
  } yield (dots zip values).toMap

  implicit def arbDotFun[A: UIJDLattice](implicit g: Gen[A]): Arbitrary[DotFun[A]] = Arbitrary(genDotFun)

  def genDotMap[K, V: DotStore](implicit gk: Gen[K], gv: Gen[V]): Gen[DotMap[K, V]] = (for {
    n      <- Gen.posNum[Int]
    keys   <- Gen.containerOfN[List, K](n, gk)
    values <- Gen.containerOfN[List, V](n, gv)
  } yield (keys zip values).toMap).suchThat { m =>
    val dotsIter = m.values.flatMap(DotStore[V].dots)
    val dotsSet  = dotsIter.toSet
    dotsIter.size == dotsSet.size
  }

  implicit def arbDotMap[K, V: DotStore](implicit gk: Gen[K], gv: Gen[V]): Arbitrary[DotMap[K, V]] =
    Arbitrary(genDotMap)
}

class DotSetTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import DotStoreGenerators._

  "dots" in forAll { ds: Set[Dot] =>
    assert(
      DotSet.dots(ds) == ds,
      s"DotSet.dots should return the set itself, but ${DotSet.dots(ds)} does not equal $ds"
    )
  }

  "empty" in assert(
    DotSet.empty.isEmpty,
    s"DotSet.empty should be empty, but ${DotSet.empty} is not empty"
  )

  "merge" in forAll { (dsA: Set[Dot], deletedA: Set[Dot], dsB: Set[Dot], deletedB: Set[Dot]) =>
    val ccA = dsA union deletedA
    val ccB = dsB union deletedB

    val Causal(dsMerged, ccMerged) = UIJDLattice[Causal[DotSet, SetCContext]].merge(Causal(dsA, ccA), Causal(dsB, ccB))

    assert(
      ccMerged == (ccA union ccB),
      s"DotSet.merge should have the same effect as set union on the causal context, but $ccMerged does not equal ${ccA union ccB}"
    )
    assert(
      dsMerged subsetOf (dsA union dsB),
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

  "leq" in forAll { (dsA: Set[Dot], deletedA: Set[Dot], dsB: Set[Dot], deletedB: Set[Dot]) =>
    val ccA = dsA union deletedA
    val ccB = dsB union deletedB

    assert(
      DotSet.leq(Causal(dsA, ccA), Causal(dsA, ccA)),
      s"DotSet.leq should be reflexive, but returns false when applied to ($dsA, $ccA, $dsA, $ccA)"
    )

    val Causal(dsMerged, ccMerged) = UIJDLattice[Causal[DotSet, SetCContext]].merge(Causal(dsA, ccA), Causal(dsB, ccB))

    assert(
      DotSet.leq(Causal(dsA, ccA), Causal(dsMerged, ccMerged)),
      s"The result of DotSet.merge should be larger than its lhs, but DotSet.leq returns false when applied to ($dsA, $ccA, $dsMerged, $ccMerged)"
    )
    assert(
      DotSet.leq(Causal(dsB, ccB), Causal(dsMerged, ccMerged)),
      s"The result of DotSet.merge should be larger than its rhs, but DotSet.leq returns false when applied to ($dsB, $ccB, $dsMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (ds: Set[Dot], deleted: Set[Dot]) =>
    val cc = ds union deleted

    val decomposed = DotSet.decompose(Causal(ds, cc))
    val Causal(dsMerged, ccMerged) = decomposed.foldLeft(Causal(Set.empty[Dot], Set.empty[Dot])) {
      case (Causal(dsA, ccA), Causal(dsB, ccB)) =>
        UIJDLattice[Causal[DotSet, SetCContext]].merge(Causal(dsA, ccA), Causal(dsB, ccB))
    }

    assert(
      dsMerged == ds,
      s"Merging the list of atoms returned by DotSet.decompose should produce an equal DotSet, but $dsMerged does not equal $ds"
    )
    assert(
      ccMerged == cc,
      s"Merging the list of atoms returned by DotSet.decompose should produce an equal Causal Context, but $ccMerged does not equal $cc"
    )
  }
}

class DotFunTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import DotStoreGenerators._

  "dots" in forAll { df: DotFun[Int] =>
    assert(
      DotFun[Int].dots(df) == df.keySet,
      s"DotFun.dots should return the keys of the DotFun itself, but ${DotFun[Int].dots(df)} does not equal $df"
    )
  }

  "empty" in assert(
    DotFun[Int].empty.isEmpty,
    s"DotFun.empty should be empty, but ${DotFun[Int].empty} is not empty"
  )

  "merge" in forAll { (dfA: DotFun[Int], deletedA: Set[Dot], dfB: DotFun[Int], deletedB: Set[Dot]) =>
    val dotsA = DotFun[Int].dots(dfA)
    val dotsB = DotFun[Int].dots(dfB)
    val ccA   = dotsA union deletedA
    val ccB   = dotsB union deletedB

    val Causal(dfMerged, ccMerged) =
      UIJDLattice[Causal[DotFun[Int], SetCContext]].merge(Causal(dfA, ccA), Causal(dfB, ccB))
    val dotsMerged = DotFun[Int].dots(dfMerged)

    assert(
      ccMerged == (ccA union ccB),
      s"DotFun.merge should have the same effect as set union on the causal context, but $ccMerged does not equal ${ccA union ccB}"
    )
    assert(
      dotsMerged subsetOf (dotsA union dotsB),
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

    (dotsA intersect dotsB).foreach { d =>
      assert(
        dfMerged(d) == UIJDLattice[Int].merge(dfA(d), dfB(d)),
        s"If a dot is used as key in both DotFuns then the corresponding values should be merged in the result of DotFun.merge, but ${dfMerged(
          d
        )} does not equal ${UIJDLattice[Int].merge(dfA(d), dfB(d))}"
      )
    }

    (dotsA diff ccB).foreach { d =>
      assert(
        dfMerged(d) == dfA(d),
        s"If a dot only appears on the lhs of DotFun.merge then resulting DotFun should have the same mapping as the lhs, but ${dfMerged(d)} does not equal ${dfA(d)}"
      )
    }

    (dotsB diff ccA).foreach { d =>
      assert(
        dfMerged(d) == dfB(d),
        s"If a dot only appears on the rhs of DotFun.merge then resulting DotFun should have the same mapping as the rhs, but ${dfMerged(d)} does not equal ${dfB(d)}"
      )
    }
  }

  "leq" in forAll { (dfA: DotFun[Int], deletedA: Set[Dot], dfB: DotFun[Int], deletedB: Set[Dot]) =>
    val ccA = DotFun[Int].dots(dfA) union deletedA
    val ccB = DotFun[Int].dots(dfB) union deletedB

    assert(
      DotFun[Int].leq(Causal(dfA, ccA), Causal(dfA, ccA)),
      s"DotFun.leq should be reflexive, but returns false when applied to ($dfA, $ccA, $dfA, $ccA)"
    )

    val Causal(dfMerged, ccMerged) =
      UIJDLattice[Causal[DotFun[Int], SetCContext]].merge(Causal(dfA, ccA), Causal(dfB, ccB))

    assert(
      DotFun[Int].leq(Causal(dfA, ccA), Causal(dfMerged, ccMerged)),
      s"The result of DotFun.merge should be larger than its lhs, but DotFun.leq returns false when applied to ($dfA, $ccA, $dfMerged, $ccMerged)"
    )
    assert(
      DotFun[Int].leq(Causal(dfB, ccB), Causal(dfMerged, ccMerged)),
      s"The result of DotFun.merge should be larger than its rhs, but DotFun.leq returns false when applied to ($dfB, $ccB, $dfMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (df: DotFun[Int], deleted: Set[Dot]) =>
    val cc = DotFun[Int].dots(df) union deleted

    val decomposed = DotFun[Int].decompose(Causal(df, cc))
    val Causal(dfMerged, ccMerged) = decomposed.foldLeft(Causal(DotFun[Int].empty, Set.empty[Dot])) {
      case (Causal(dfA, ccA), Causal(dfB, ccB)) =>
        UIJDLattice[Causal[DotFun[Int], SetCContext]].merge(Causal(dfA, ccA), Causal(dfB, ccB))
    }

    assert(
      dfMerged == df,
      s"Merging the list of atoms returned by DotFun.decompose should produce an equal DotFun, but $dfMerged does not equal $df"
    )
    assert(
      ccMerged == cc,
      s"Merging the list of atoms returned by DotFun.decompose should produce an equal Causal Context, but $ccMerged does not equal $cc"
    )
  }
}

class DotMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import DotStoreGenerators._

  "dots" in forAll { dm: DotMap[Int, DotSet] =>
    assert(
      DotMap[Int, DotSet].dots(dm) == dm.values.flatMap(DotSet.dots).toSet,
      s"DotMap.dots should return the keys of the DotMap itself, but ${DotMap[Int, DotSet].dots(dm)} does not equal $dm"
    )
  }

  "empty" in assert(
    DotMap[Int, DotSet].empty.isEmpty,
    s"DotMap.empty should be empty, but ${DotMap[Int, DotSet].empty} is not empty"
  )

  "merge" in forAll { (dmA: DotMap[Int, DotSet], deletedA: Set[Dot], dmB: DotMap[Int, DotSet], deletedB: Set[Dot]) =>
    val dotsA = DotMap[Int, DotSet].dots(dmA)
    val dotsB = DotMap[Int, DotSet].dots(dmB)
    val ccA   = dotsA union deletedA
    val ccB   = dotsB union deletedB

    val Causal(dmMerged, ccMerged) =
      UIJDLattice[Causal[DotMap[Int, DotSet], SetCContext]].merge(Causal(dmA, ccA), Causal(dmB, ccB))
    val dotsMerged = DotMap[Int, DotSet].dots(dmMerged)

    assert(
      ccMerged == (ccA union ccB),
      s"DotMap.merge should have the same effect as set union on the causal context, but $ccMerged does not equal ${ccA union ccB}"
    )
    assert(
      dotsMerged subsetOf (dotsA union dotsB),
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

    (dmA.keySet union dmB.keySet).foreach { k =>
      val vMerged =
        DotSet.mergePartial(Causal(dmA.getOrElse(k, DotSet.empty), ccA), Causal(dmB.getOrElse(k, DotSet.empty), ccB))

      assert(
        vMerged.isEmpty || dmMerged(k) == vMerged,
        s"For all keys that are in both DotMaps the result of DotMap.merge should map these to the merged values, but ${dmMerged.get(k)} does not equal $vMerged"
      )
    }
  }

  "leq".taggedAs(IgnoreOnGithubCiBecause("flaky tests, probably buggy, but not useful to know in CI")) in forAll { (dmA: DotMap[Int, DotSet], deletedA: Set[Dot], dmB: DotMap[Int, DotSet], deletedB: Set[Dot]) =>
    val ccA = DotMap[Int, DotSet].dots(dmA) union deletedA
    val ccB = DotMap[Int, DotSet].dots(dmB) union deletedB

    assert(
      DotMap[Int, DotSet].leq(Causal(dmA, ccA), Causal(dmA, ccA)),
      s"DotMap.leq should be reflexive, but returns false when applied to ($dmA, $ccA, $dmA, $ccA)"
    )

    val Causal(dmMerged, ccMerged) =
      UIJDLattice[Causal[DotMap[Int, DotSet], SetCContext]].merge(Causal(dmA, ccA), Causal(dmB, ccB))

    assert(
      DotMap[Int, DotSet].leq(Causal(dmA, ccA), Causal(dmMerged, ccMerged)),
      s"The result of DotMap.merge should be larger than its lhs, but DotMap.leq returns false when applied to ($dmA, $ccA, $dmMerged, $ccMerged)"
    )
    assert(
      DotMap[Int, DotSet].leq(Causal(dmB, ccB), Causal(dmMerged, ccMerged)),
      s"The result of DotMap.merge should be larger than its rhs, but DotMap.leq returns false when applied to ($dmB, $ccB, $dmMerged, $ccMerged)"
    )
  }

  "decompose" in forAll { (dm: DotMap[Int, DotSet], deleted: Set[Dot]) =>
    val cc = DotMap[Int, DotSet].dots(dm) union deleted

    val decomposed = DotMap[Int, DotSet].decompose(Causal(dm, cc))
    val Causal(dmMerged, ccMerged) = decomposed.foldLeft(Causal(DotMap[Int, DotSet].empty, Set.empty[Dot])) {
      case (Causal(dmA, ccA), Causal(dmB, ccB)) =>
        UIJDLattice[Causal[DotMap[Int, DotSet], SetCContext]].merge(Causal(dmA, ccA), Causal(dmB, ccB))
    }

    val dotsIter      = dm.values.flatMap(DotSet.dots)
    val dotsSet       = dotsIter.toSet
    val duplicateDots = dotsIter.size != dotsSet.size

    assert(
      ccMerged == cc,
      s"Merging the list of atoms returned by DotMap.decompose should produce an equal DotMap, but $dmMerged does not equal $dm"
    )
    assert(
      duplicateDots || dm.keys.forall(k => dm(k) == dmMerged.getOrElse(k, DotSet.empty)),
      s"Merging the list of atoms returned by DotMap.decompose should produce an equal Causal Context, but $ccMerged does not equal $cc"
    )
  }
}
