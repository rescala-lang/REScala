package test.kofre

import kofre.base.{Lattice, Uid}
import kofre.dotted.{DotFun, Dotted, DottedLattice, HasDots}
import kofre.time.{Dot, Dots}
import org.scalacheck.Prop.*
import test.kofre.DataGenerator.*
import test.kofre.bespoke.given

class DotFunTest extends munit.ScalaCheckSuite {

  property("dots") {
    forAll { (df: DotFun[Int]) =>
      assert(
        df.dots.toSet == df.repr.keySet,
        s"DotFun.dots should return the keys of the DotFun itself, but ${df.dots} does not equal $df"
      )
    }
  }

  test("empty") {
    assert(
      DotFun.empty[Int].repr.isEmpty,
      s"DotFun.empty should be empty, but ${DotFun.empty[Int]} is not empty"
    )
  }

  property("merge") {
    forAll { (dfA: DotFun[Int], deletedA: Dots, dfB: DotFun[Int], deletedB: Dots) =>
      val dotsA = dfA.dots
      val dotsB = dfB.dots
      val ccA   = dotsA union deletedA
      val ccB   = dotsB union deletedB

      val Dotted(dfMerged, ccMerged) =
        Lattice[Dotted[DotFun[Int]]].merge(
          Dotted(dfA, ccA),
          Dotted(dfB, ccB)
        )
      val dotsMerged = dfMerged.dots

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
          dfMerged.repr(d) == Lattice[Int].merge(dfA.repr(d), dfB.repr(d)),
          s"If a dot is used as key in both DotFuns then the corresponding values should be merged in the result of DotFun.merge, but ${dfMerged.repr(
              d
            )} does not equal ${Lattice[Int].merge(dfA.repr(d), dfB.repr(d))}"
        )
      }

      (dotsA diff ccB).iterator.foreach { d =>
        assert(
          dfMerged.repr(d) == dfA.repr(d),
          s"If a dot only appears on the lhs of DotFun.merge then resulting DotFun should have the same mapping as the lhs, but ${dfMerged.repr(d)} does not equal ${dfA.repr(d)}"
        )
      }

      (dotsB diff ccA).iterator.foreach { d =>
        assert(
          dfMerged.repr(d) == dfB.repr(d),
          s"If a dot only appears on the rhs of DotFun.merge then resulting DotFun should have the same mapping as the rhs, but ${dfMerged.repr(d)} does not equal ${dfB.repr(d)}"
        )
      }
    }
  }

  property("leq") {
    forAll { (dfA: DotFun[Int], deletedA: Dots, dfB: DotFun[Int], deletedB: Dots) =>
      val ccA = dfA.dots union deletedA
      val ccB = dfB.dots union deletedB

      assert(
        DotFun.dottedLattice[Int].lteq(Dotted(dfA, ccA), Dotted(dfA, ccA)),
        s"DotFun.leq should be reflexive, but returns false when applied to ($dfA, $ccA, $dfA, $ccA)"
      )

      val Dotted(dfMerged, ccMerged) =
        DottedLattice[DotFun[Int]].merge(
          Dotted(dfA, (ccA)),
          Dotted(dfB, (ccB))
        )

      assert(
        DotFun.dottedLattice[Int].lteq(Dotted(dfA, (ccA)), Dotted(dfMerged, ccMerged)),
        s"The result of DotFun.merge should be larger than its lhs, but DotFun.leq returns false when applied to ($dfA, $ccA, $dfMerged, $ccMerged)"
      )
      assert(
        DotFun.dottedLattice[Int].lteq(Dotted(dfB, (ccB)), Dotted(dfMerged, ccMerged)),
        s"The result of DotFun.merge should be larger than its rhs, but DotFun.leq returns false when applied to ($dfB, $ccB, $dfMerged, $ccMerged)"
      )
    }
  }

  test("deletions are larger") {
    val ia = Uid.gen()
    val ib = Uid.gen()
    val someDot = Dot(ia, 1)
    val left = Dotted(DotFun(Map(someDot -> 10)), Dots.single(someDot))
    val right = Dotted(DotFun.empty[Int], Dots.single(someDot))

    assert(left <= right)

  }

  test("decompose") {
    type D = Dotted[DotFun[Set[Int]]]

    val dot = Dot("a", 0)
    val cc  = Dots.from(Set(dot))

    val data =
      Dotted[DotFun[Set[Int]]](DotFun(Map(dot -> Set(1, 2, 3))), cc)
    val dec: Iterable[D] = data.decomposed
    val rec              = dec.reduceLeft(_ merge _)

    assertEquals(rec, data)
  }

  property("decompose recompose") {
    forAll { (df: DotFun[Int], deleted: Dots) =>
      val cc = df.dots union deleted

      val withContext = Dotted(df, cc)

      val decomposed: Iterable[Dotted[DotFun[Int]]] = DotFun.dottedLattice.decompose(withContext)

      decomposed.foreach { dec =>
        assert(dec <= withContext)
      }

      val Dotted(dfMerged, ccMerged) =
        decomposed.foldLeft(Dotted(DotFun.empty[Int], Dots.empty)) {
          case (Dotted(dfA, ccA), Dotted(dfB, ccB)) =>
            Lattice[Dotted[DotFun[Int]]].merge(Dotted(dfA, ccA), Dotted(dfB, ccB))
        }

      assertEquals(dfMerged, df)
      assertEquals(
        ccMerged,
        cc, {
          val decc     = decomposed.filter(_.context.rangeAt("c").isEmpty.unary_!)
          val contexts = decc.map(_.context)
          s"${cc.contains(Dot("c", 78))}, ${ccMerged.contains(Dot("c", 78))}, \n${contexts}\n${cc.rangeAt(
              "c"
            )}\n${contexts.reduceLeft(_ merge _)}\n${decc}\n${decc.mkString("--", "\n--", "")}"
        }
      )
    }
  }
}
