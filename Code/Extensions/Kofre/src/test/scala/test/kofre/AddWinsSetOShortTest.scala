package test.kofre

import kofre.dotbased.AddWinsSetO
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.Seq


class AddWinsSetOShortTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  implicit val shortlists: Arbitrary[Seq[Int]] = Arbitrary(Gen.someOf(0 to 20))

  "new old merge equals" in forAll { (list1: Seq[Int], list2: Seq[Int]) =>
    val replica = "rep1"
    val aws1    = list1.foldLeft(AddWinsSetO.empty[Int])(_.add(_, replica))
    val aws2    = list2.foldLeft(AddWinsSetO.empty[Int])(_.add(_, replica))

    val mergeOld = AddWinsSetO.latticeAddWinsSet.merge(aws1, aws2)
    val mergeNew = AddWinsSetO.latticeAddWinsSetPerfOpt.merge(aws1, aws2)

    assert(mergeOld == mergeNew)
  }

}
