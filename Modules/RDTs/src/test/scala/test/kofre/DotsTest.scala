package test.kofre

import kofre.base.Uid
import kofre.time.{Dot, Dots}
import org.scalacheck.Prop.*
import test.kofre.DataGenerator.{*, given}

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
      val ids   = asSet.map(_.replicaId) + randId

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
      val ids   = asSet.map(_.replicaId) + randId

      ids.foreach { id =>
        val nd = cc.nextDot(id)

        assert(
          nd.replicaId == id,
          s"DietMapCContext.nextDot should return a dot for the given replicaID, but ${nd.replicaId} does not equal $id"
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
}
