package test.rdts.corestructs

import org.scalacheck.Prop.*
import rdts.base.Uid
import rdts.time.{Dot, Dots, ArrayRanges, Time}
import test.rdts.DataGenerator.given

class DotsTest extends munit.ScalaCheckSuite {

  test("empty") {
    assert(
      Dots.empty.toSet.isEmpty,
      s"DietMapCContext.empty should be empty, but ${Dots.empty} is not empty"
    )
  }

  property("contains") {
    forAll { (cc: Dots) =>
      (cc.toSet).foreach { d =>
        assert(
          cc.contains(d),
          s"DietMapCContext.contains should return true for every dot in the context, but returns false when applied to ($cc, $d)"
        )
      }
    }
  }

  property("to/fromSet") {
    forAll { (ds: Set[Dot]) =>
      assert(
        Dots.from(ds).toSet == ds,
        s"DietMapCContext.toSet and DietMapCContext.from should be inverse operations, but ${Dots.from(ds).toSet} does not equal $ds"
      )
    }
  }

  property("union") {
    forAll { (cca: Dots, ccb: Dots) =>
      val ccunion = (cca union ccb)

      val seta     = cca.toSet
      val setb     = ccb.toSet
      val setunion = ccunion.toSet

      assert(
        setunion == (seta union setb),
        s"DietMapCContext.union should be equivalent to set union, but $setunion does not equal ${seta union setb}"
      )
    }
  }

  property("max") {
    forAll { (cc: Dots, randId: Uid) =>
      val asSet = cc.toSet
      val ids   = asSet.map(_.place) + randId

      ids.foreach { id =>
        val counterVals = asSet.collect {
          case Dot(i, n) if i == id => n
        }

        cc.max(id) match {
          case Some(Dot(i, c)) =>
            assert(
              i == id,
              s"DietMapCContext.max should return a dot that has the same replicaID as the argument, but $i does not equal $id"
            )
            assert(
              counterVals.forall(_ <= c),
              s"The counter value of the dot returned by DietMapCContext.max should be larger that those of all other dots with the same replicaID, but $counterVals contains a counter that is larger than $c"
            )
          case None =>
            assert(
              id == randId,
              s"DietMapCContext.max should only return None if there are no dots with the given id in the causal context, but for $id there is a dot in $cc"
            )
        }
      }
    }
  }

  property("nextDot") {
    forAll { (cc: Dots, randId: Uid) =>
      val asSet = (cc.toSet)
      val ids   = asSet.map(_.place) + randId

      ids.foreach { id =>
        val nd = cc.nextDot(id)

        assert(
          nd.place == id,
          s"DietMapCContext.nextDot should return a dot for the given replicaID, but ${nd.place} does not equal $id"
        )

        cc.max(id) match {
          case Some(Dot(_, c)) =>
            assert(
              nd.time == c + 1,
              s"DietMapCContext.nextDot should return a dot whose counter is 1 higher than that of DietMapCContext.max, but ${nd.time} does not equal $c + 1 (cc is $cc)"
            )
          case None =>
            assert(
              nd.time == 0,
              s"If there exists no maximal dot then DietMapCContext.nextDot should return a dot whose counter is 0, but ${nd.time} does not equal 0"
            )
        }
      }
    }
  }

  property("<=") {
    forAll { (left: Dots, right: Dots) =>
      val leftSet  = left.toSet
      val rightSet = right.toSet
      if left <= right then
        assertEquals(leftSet.intersect(rightSet), leftSet, s"left: $left\nright: $right")
        assertEquals(leftSet.union(rightSet), rightSet)

      assertEquals(left.disjunct(right), leftSet.intersect(rightSet).isEmpty)
    }
  }

  test("head throws with empty Dots") {
    intercept[NoSuchElementException](Dots(Map.empty).head)
    intercept[NoSuchElementException](Dots(Map(Uid("a") -> ArrayRanges.empty)).head)
    intercept[NoSuchElementException](
      Dots(Map(
        Uid("a") -> ArrayRanges.empty,
        Uid("b") -> ArrayRanges.empty
      )).head
    )
  }

  test("head works with nonempty Dots") {
    assertEquals(Dots(Map(Uid("a") -> new ArrayRanges(Array(1, 3, 6, 9), 4))).head, Dot(Uid("a"), 1))

    assertEquals(Dots(Map(Uid("a") -> ArrayRanges.empty, Uid("b") -> new ArrayRanges(Array(1, 3, 6, 9), 4))).head, Dot(Uid("b"), 1))
  }
}
