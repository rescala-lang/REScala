package test.kofre

import kofre.base.DecomposeLattice
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.{AsCausalContext, WithContext}
import kofre.dotted.DotFun
import org.scalacheck.Prop.*
import test.kofre.DataGenerator.*

class DotFunTest extends munit.ScalaCheckSuite {

  property("dots") {
    forAll { (df: DotFun[Int]) =>
      assert(
        AsCausalContext[DotFun[Int]].dots(df).toSet == df.keySet,
        s"DotFun.dots should return the keys of the DotFun itself, but ${AsCausalContext[DotFun[Int]].dots(df)} does not equal $df"
      )
    }
  }

  test("empty") {
    assert(
      DotFun.empty[Int].isEmpty,
      s"DotFun.empty should be empty, but ${DotFun.empty[Int]} is not empty"
    )
  }

  property("merge") {
    forAll { (dfA: DotFun[Int], deletedA: CausalContext, dfB: DotFun[Int], deletedB: CausalContext) =>
      val dotsA = AsCausalContext[DotFun[Int]].dots(dfA)
      val dotsB = AsCausalContext[DotFun[Int]].dots(dfB)
      val ccA   = dotsA union deletedA
      val ccB   = dotsB union deletedB

      val WithContext(dfMerged, ccMerged) =
        DecomposeLattice[WithContext[DotFun[Int]]].merge(
          WithContext(dfA, ccA),
          WithContext(dfB, ccB)
        )
      val dotsMerged = AsCausalContext[DotFun[Int]].dots(dfMerged)

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
          dfMerged(d) == DecomposeLattice[Int].merge(dfA(d), dfB(d)),
          s"If a dot is used as key in both DotFuns then the corresponding values should be merged in the result of DotFun.merge, but ${dfMerged(
              d
            )} does not equal ${DecomposeLattice[Int].merge(dfA(d), dfB(d))}"
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
  }

  property("leq") {
    forAll { (dfA: DotFun[Int], deletedA: CausalContext, dfB: DotFun[Int], deletedB: CausalContext) =>
      val ccA = AsCausalContext[DotFun[Int]].dots(dfA) union deletedA
      val ccB = AsCausalContext[DotFun[Int]].dots(dfB) union deletedB

      assert(
        DotFun.perDotDecompose[Int].lteq(WithContext(dfA, ccA), WithContext(dfA, ccA)),
        s"DotFun.leq should be reflexive, but returns false when applied to ($dfA, $ccA, $dfA, $ccA)"
      )

      val WithContext(dfMerged, ccMerged) =
        DecomposeLattice[WithContext[DotFun[Int]]].merge(
          WithContext(dfA, (ccA)),
          WithContext(dfB, (ccB))
        )

      assert(
        DotFun.perDotDecompose[Int].lteq(WithContext(dfA, (ccA)), WithContext(dfMerged, ccMerged)),
        s"The result of DotFun.merge should be larger than its lhs, but DotFun.leq returns false when applied to ($dfA, $ccA, $dfMerged, $ccMerged)"
      )
      assert(
        DotFun.perDotDecompose[Int].lteq(WithContext(dfB, (ccB)), WithContext(dfMerged, ccMerged)),
        s"The result of DotFun.merge should be larger than its rhs, but DotFun.leq returns false when applied to ($dfB, $ccB, $dfMerged, $ccMerged)"
      )
    }
  }

  test("decompose") {
    type D = WithContext[DotFun[Set[Int]]]

    val dot = Dot("a", 0)
    val cc  = CausalContext.fromSet(Set(dot))

    val data =
      WithContext[DotFun[Set[Int]]](DotFun(Map(dot -> Set(1, 2, 3))), cc)
    val dec: Iterable[D] = data.decomposed
    val rec              = dec.reduceLeft(_ merged _)

    assertEquals(rec, data)
  }

  property("decompose recompose") {
    forAll { (df: DotFun[Int], deleted: CausalContext) =>
      val cc = AsCausalContext[DotFun[Int]].dots(df) union deleted

      val withContext = WithContext(df, cc)

      val decomposed: Iterable[WithContext[DotFun[Int]]] = DotFun.perDotDecompose.decompose(withContext)

      decomposed.foreach { dec =>
        assert(dec <= withContext)
      }

      val WithContext(dfMerged, ccMerged) =
        decomposed.foldLeft(WithContext(DotFun.empty[Int], CausalContext.empty)) {
          case (WithContext(dfA, ccA), WithContext(dfB, ccB)) =>
            DecomposeLattice[WithContext[DotFun[Int]]].merge(WithContext(dfA, ccA), WithContext(dfB, ccB))
        }

      assertEquals(dfMerged, df)
      assertEquals(
        ccMerged,
        cc, {
          val decc     = decomposed.filter(_.context.rangeAt("c").isEmpty.unary_!)
          val contexts = decc.map(_.context)
          s"${cc.contains(Dot("c", 78))}, ${ccMerged.contains(Dot("c", 78))}, \n${contexts}\n${cc.rangeAt(
              "c"
            )}\n${contexts.reduceLeft(_ merged _)}\n${decc}\n${decc.mkString("--", "\n--", "")}"
        }
      )
    }
  }
}
