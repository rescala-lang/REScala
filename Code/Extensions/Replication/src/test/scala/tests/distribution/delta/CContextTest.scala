package tests.distribution.delta

import kofre.causality.{CContext, CausalContext, Dot}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

object CContextGenerators {
  import DotStoreGenerators._

  val genSetCContext: Gen[Set[Dot]] = genDotSet

  implicit val arbSetCContext: Arbitrary[Set[Dot]] = Arbitrary(genSetCContext)

  val genDietMapCContext: Gen[CausalContext] = for {
    ds <- genDotSet
  } yield CausalContext.fromSet(ds)

  implicit val arbDietMapCContext: Arbitrary[CausalContext] = Arbitrary(genDietMapCContext)
}

class SetCContextTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import CContextGenerators._

  val SetCContext = CContext.intTreeCC

  "empty" in assert(
    CausalContext.empty.toSet.isEmpty,
    s"SetCContext.empty should be empty, but ${CausalContext.empty} is not empty"
  )

  "contains" in forAll { cc: CausalContext =>
    cc.toSet.foreach { d =>
      assert(
        SetCContext.contains(cc, d),
        s"SetCContext.contains should return true for every dot in the context, but returns false when applied to ($cc, $d)"
      )
    }
  }

  "to/fromSet" in forAll { ds: Set[Dot] =>
    assert(
      SetCContext.toSet(SetCContext.fromSet(ds)) == ds,
      s"SetCContext.toSet and SetCContext.fromSet should be inverse operations, but ${SetCContext.toSet(SetCContext.fromSet(ds))} does not equal $ds"
    )
  }

  "union" in forAll { (cca: CausalContext, ccb: CausalContext) =>
    val ccunion = SetCContext.union(cca, ccb)

    val seta     = SetCContext.toSet(cca)
    val setb     = SetCContext.toSet(ccb)
    val setunion = SetCContext.toSet(ccunion)

    assert(
      setunion == (seta union setb),
      s"SetCContext.union should be equivalent to set union, but $setunion does not equal ${seta union setb}"
    )
  }

  "max" in forAll { (cc: CausalContext, randId: String) =>
    val asSet = SetCContext.toSet(cc)
    val ids   = asSet.map(_.replicaID) + randId

    ids.foreach { id =>
      val counterVals = asSet.collect {
        case Dot(i, n) if i == id => n
      }

      SetCContext.max(cc, id) match {
        case Some(Dot(i, c)) =>
          assert(
            i == id,
            s"SetCContext.max should return a dot that has the same replicaID as the argument, but $i does not equal $id"
          )
          assert(
            counterVals.forall(_ <= c),
            s"The counter value of the dot returned by SetCContext.max should be larger that those of all other dots with the same replicaID, but $counterVals contains a counter that is larger than $c"
          )
        case None =>
          assert(
            id == randId,
            s"SetCContext.max should only return None if there are no dots with the given id in the causal context, but for $id there is a dot in $cc"
          )
      }
    }
  }

  "nextDot" in forAll { (cc: CausalContext, randId: String) =>
    val asSet = SetCContext.toSet(cc)
    val ids   = asSet.map(_.replicaID) + randId

    ids.foreach { id =>
      val nd = SetCContext.nextDot(cc, id)

      assert(
        nd.replicaID == id,
        s"SetCContext.nextDot should return a dot for the given replicaID, but ${nd.replicaID} does not equal $id"
      )

      SetCContext.max(cc, id) match {
        case Some(Dot(_, c)) =>
          assert(
            nd.time == c + 1,
            s"SetCContext.nextDot should return a dot whose counter is 1 higher than that of SetCContext.max, but ${nd.time} does not equal $c + 1"
          )
        case None =>
          assert(
            nd.time == 0,
            s"If there exists no maximal dot then SetCContext.nextDot should return a dot whose counter is 0, but ${nd.time} does not equal 0"
          )
      }
    }
  }
}

class DietMapCContextTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import CContextGenerators._

  val DietMapCContext = CContext.intTreeCC

  "empty" in assert(
    DietMapCContext.toSet(DietMapCContext.empty).isEmpty,
    s"DietMapCContext.empty should be empty, but ${DietMapCContext.empty} is not empty"
  )

  "contains" in forAll { cc: CausalContext =>
    DietMapCContext.toSet(cc).foreach { d =>
      assert(
        DietMapCContext.contains(cc, d),
        s"DietMapCContext.contains should return true for every dot in the context, but returns false when applied to ($cc, $d)"
      )
    }
  }

  "to/fromSet" in forAll { ds: Set[Dot] =>
    assert(
      DietMapCContext.toSet(DietMapCContext.fromSet(ds)) == ds,
      s"DietMapCContext.toSet and DietMapCContext.fromSet should be inverse operations, but ${DietMapCContext.toSet(DietMapCContext.fromSet(ds))} does not equal $ds"
    )
  }

  "union" in forAll { (cca: CausalContext, ccb: CausalContext) =>
    val ccunion = DietMapCContext.union(cca, ccb)

    val seta     = DietMapCContext.toSet(cca)
    val setb     = DietMapCContext.toSet(ccb)
    val setunion = DietMapCContext.toSet(ccunion)

    assert(
      setunion == (seta union setb),
      s"DietMapCContext.union should be equivalent to set union, but $setunion does not equal ${seta union setb}"
    )
  }

  "max" in forAll { (cc: CausalContext, randId: String) =>
    val asSet = DietMapCContext.toSet(cc)
    val ids   = asSet.map(_.replicaID) + randId

    ids.foreach { id =>
      val counterVals = asSet.collect {
        case Dot(i, n) if i == id => n
      }

      DietMapCContext.max(cc, id) match {
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

  "nextDot" in forAll { (cc: CausalContext, randId: String) =>
    val asSet = DietMapCContext.toSet(cc)
    val ids   = asSet.map(_.replicaID) + randId

    ids.foreach { id =>
      val nd = DietMapCContext.nextDot(cc, id)

      assert(
        nd.replicaID == id,
        s"DietMapCContext.nextDot should return a dot for the given replicaID, but ${nd.replicaID} does not equal $id"
      )

      DietMapCContext.max(cc, id) match {
        case Some(Dot(_, c)) =>
          assert(
            nd.time == c + 1,
            s"DietMapCContext.nextDot should return a dot whose counter is 1 higher than that of DietMapCContext.max, but ${nd.time} does not equal $c + 1"
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
