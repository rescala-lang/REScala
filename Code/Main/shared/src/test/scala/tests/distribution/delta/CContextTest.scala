package tests.distribution.delta

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.CContext.{DietMapCContext, SetCContext}
import rescala.extra.lattices.delta.Dot

object CContextGenerators {
  import DotStoreGenerators._

  val genSetCContext: Gen[SetCContext] = genDotSet

  implicit val arbSetCContext: Arbitrary[SetCContext] = Arbitrary(genSetCContext)

  val genDietMapCContext: Gen[DietMapCContext] = for {
    ds <- genDotSet
  } yield DietMapCContext.fromSet(ds)

  implicit val arbDietMapCContext: Arbitrary[DietMapCContext] = Arbitrary(genDietMapCContext)
}

class SetCContextTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import CContextGenerators._

  "empty" in assert(
    SetCContext.toSet(SetCContext.empty).isEmpty,
    s"SetCContext.empty should be empty, but ${SetCContext.empty} is not empty"
  )

  "contains" in forAll { cc: SetCContext =>
    SetCContext.toSet(cc).foreach { d =>
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

  "union" in forAll { (cca: SetCContext, ccb: SetCContext) =>
    val ccunion = SetCContext.union(cca, ccb)

    val seta = SetCContext.toSet(cca)
    val setb = SetCContext.toSet(ccb)
    val setunion = SetCContext.toSet(ccunion)

    assert(
      setunion == (seta union setb),
      s"SetCContext.union should be equivalent to set union, but $setunion does not equal ${seta union setb}"
    )
  }

  "max" in forAll { (cc: SetCContext, randId: String) =>
    val asSet = SetCContext.toSet(cc)
    val ids = asSet.map(_.replicaID) + randId

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

  "nextDot" in forAll { (cc: SetCContext, randId: String) =>
    val asSet = SetCContext.toSet(cc)
    val ids = asSet.map(_.replicaID) + randId

    ids.foreach { id =>
      val nd = SetCContext.nextDot(cc, id)

      assert(
        nd.replicaID == id,
        s"SetCContext.nextDot should return a dot for the given replicaID, but ${nd.replicaID} does not equal $id"
      )

      SetCContext.max(cc, id) match {
        case Some(Dot(_, c)) =>
          assert(
            nd.counter == c + 1,
            s"SetCContext.nextDot should return a dot whose counter is 1 higher than that of SetCContext.max, but ${nd.counter} does not equal $c + 1"
          )
        case None =>
          assert(
            nd.counter == 0,
            s"If there exists no maximal dot then SetCContext.nextDot should return a dot whose counter is 0, but ${nd.counter} does not equal 0"
          )
      }
    }
  }
}

class DietMapCContextTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import CContextGenerators._

  "empty" in assert(
    DietMapCContext.toSet(DietMapCContext.empty).isEmpty,
    s"DietMapCContext.empty should be empty, but ${DietMapCContext.empty} is not empty"
  )

  "contains" in forAll { cc: DietMapCContext =>
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

  "union" in forAll { (cca: DietMapCContext, ccb: DietMapCContext) =>
    val ccunion = DietMapCContext.union(cca, ccb)

    val seta = DietMapCContext.toSet(cca)
    val setb = DietMapCContext.toSet(ccb)
    val setunion = DietMapCContext.toSet(ccunion)

    assert(
      setunion == (seta union setb),
      s"DietMapCContext.union should be equivalent to set union, but $setunion does not equal ${seta union setb}"
    )
  }

  "max" in forAll { (cc: DietMapCContext, randId: String) =>
    val asSet = DietMapCContext.toSet(cc)
    val ids = asSet.map(_.replicaID) + randId

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

  "nextDot" in forAll { (cc: DietMapCContext, randId: String) =>
    val asSet = DietMapCContext.toSet(cc)
    val ids = asSet.map(_.replicaID) + randId

    ids.foreach { id =>
      val nd = DietMapCContext.nextDot(cc, id)

      assert(
        nd.replicaID == id,
        s"DietMapCContext.nextDot should return a dot for the given replicaID, but ${nd.replicaID} does not equal $id"
      )

      DietMapCContext.max(cc, id) match {
        case Some(Dot(_, c)) =>
          assert(
            nd.counter == c + 1,
            s"DietMapCContext.nextDot should return a dot whose counter is 1 higher than that of DietMapCContext.max, but ${nd.counter} does not equal $c + 1"
          )
        case None =>
          assert(
            nd.counter == 0,
            s"If there exists no maximal dot then DietMapCContext.nextDot should return a dot whose counter is 0, but ${nd.counter} does not equal 0"
          )
      }
    }
  }
}

class MixedCContextTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import CContextGenerators._

  "union" in forAll { (cca: SetCContext, ccb: DietMapCContext) =>
    val ccuniona = SetCContext.union(cca, ccb)
    val ccunionb = DietMapCContext.union(ccb, cca)

    val seta = SetCContext.toSet(cca)
    val setb = DietMapCContext.toSet(ccb)
    val setuniona = SetCContext.toSet(ccuniona)
    val setunionb = DietMapCContext.toSet(ccunionb)

    assert(
      setuniona == (seta union setb),
      s"SetCContext.union should be equivalent to set union, but $setuniona does not equal ${seta union setb}"
    )
    assert(
      setunionb == (seta union setb),
      s"DietMapCContext.union should be equivalent to set union, but $setunionb does not equal ${seta union setb}"
    )
  }

  "convert" in forAll { cc: SetCContext =>
    val asSet = SetCContext.toSet(cc)
    val ccConverted = SetCContext.convert[DietMapCContext](cc)
    val convertedAsSet = DietMapCContext.toSet(ccConverted)
    val ccDoubleConverted = DietMapCContext.convert[SetCContext](ccConverted)

    assert(
      ccDoubleConverted == cc,
      s"Converting back and forth between SetCContext and DietMapCContext should yield the same causal context, but $ccDoubleConverted does not equal $cc"
    )
    assert(
      convertedAsSet == asSet,
      s"The converted causal should contain the same set of dots as the original, but $convertedAsSet does not equal $asSet"
    )
  }
}
