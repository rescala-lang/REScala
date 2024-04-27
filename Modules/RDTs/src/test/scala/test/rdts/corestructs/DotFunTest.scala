package test.rdts.corestructs

import org.scalacheck.Prop.*
import rdts.base.{Lattice, Uid}
import rdts.dotted.HasDots.mapInstance
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}
import test.rdts.DataGenerator.{*, given}
import test.rdts.bespoke.given
import test.rdts.given

class DotFunTest extends munit.ScalaCheckSuite {

  property("dots") {
    forAll { (df: Map[Dot, Int]) =>
      assert(
        df.dots.toSet == df.keySet,
        s"DotFun.dots should return the keys of the DotFun itself, but ${df.dots} does not equal $df"
      )
    }
  }

  test("empty") {
    assert(
      Map.empty[Dot, Int].isEmpty,
      s"DotFun.empty should be empty, but ${Map.empty[Dot, Int]} is not empty"
    )
  }

  property("merge") {
    forAll { (dfA: Map[Dot, Int], deletedA: Dots, dfB: Map[Dot, Int], deletedB: Dots) =>
      val dotsA = dfA.dots
      val dotsB = dfB.dots
      val ccA   = dotsA union deletedA
      val ccB   = dotsB union deletedB

      val Dotted(dfMerged, ccMerged) =
        Lattice[Dotted[Map[Dot, Int]]].merge(
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
          dfMerged(d) == Lattice[Int].merge(dfA(d), dfB(d)),
          s"If a dot is used as key in both DotFuns then the corresponding values should be merged in the result of DotFun.merge, but ${dfMerged(
              d
            )} does not equal ${Lattice[Int].merge(dfA(d), dfB(d))}"
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
    forAll { (dfA: Map[Dot, Int], deletedA: Dots, dfB: Map[Dot, Int], deletedB: Dots) =>
      val ccA = dfA.dots union deletedA
      val ccB = dfB.dots union deletedB

      assert(
        Lattice[Dotted[Map[Dot, Int]]].lteq(Dotted(dfA, ccA), Dotted(dfA, ccA)),
        s"DotFun.leq should be reflexive, but returns false when applied to ($dfA, $ccA, $dfA, $ccA)"
      )

      val Dotted(dfMerged, ccMerged) =
        Lattice[Dotted[Map[Dot, Int]]].merge(
          Dotted(dfA, (ccA)),
          Dotted(dfB, (ccB))
        )

      assert(
        Lattice[Dotted[Map[Dot, Int]]].lteq(Dotted(dfA, (ccA)), Dotted(dfMerged, ccMerged)),
        s"The result of DotFun.merge should be larger than its lhs, but DotFun.leq returns false when applied to ($dfA, $ccA, $dfMerged, $ccMerged)"
      )
      assert(
        Lattice[Dotted[Map[Dot, Int]]].lteq(Dotted(dfB, (ccB)), Dotted(dfMerged, ccMerged)),
        s"The result of DotFun.merge should be larger than its rhs, but DotFun.leq returns false when applied to ($dfB, $ccB, $dfMerged, $ccMerged)"
      )
    }
  }

  test("deletions are larger") {
    val ia      = Uid.gen()
    val ib      = Uid.gen()
    val someDot = Dot(ia, 1)
    val left    = Dotted(Map(someDot -> 10), Dots.single(someDot))
    val right   = Dotted(Map.empty[Dot, Int], Dots.single(someDot))

    assert(left <= right)

  }

  test("decompose") {
    type D = Dotted[Map[Dot, Set[Int]]]

    val dot = Dot("a", 0)
    val cc  = Dots.from(Set(dot))

    val data =
      Dotted[Map[Dot, Set[Int]]](Map(dot -> Set(1, 2, 3)), cc)
    val dec: Iterable[D] = data.decomposed
    val rec              = dec.reduceLeft(_ merge _)

    assertEquals(rec, data)
  }

  property("decompose recompose") {
    forAll { (df: Map[Dot, Int], deleted: Dots) =>
      val cc = df.dots union deleted

      val withContext = Dotted(df, cc)

      val decomposed: Iterable[Dotted[Map[Dot, Int]]] = Lattice.decompose(withContext)

      decomposed.foreach { dec =>
        assert(dec <= withContext)
      }

      val Dotted(dfMerged, ccMerged) =
        decomposed.foldLeft(Dotted(Map.empty[Dot, Int], Dots.empty)) {
          case (Dotted(dfA, ccA), Dotted(dfB, ccB)) =>
            Lattice[Dotted[Map[Dot, Int]]].merge(Dotted(dfA, ccA), Dotted(dfB, ccB))
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
