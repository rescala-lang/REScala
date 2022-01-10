package test.kofre

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import kofre.dotbased.{AddWinsSetO, IntTree}

import scala.collection.Seq

object IntRangeGenerator {
  implicit val genDot: Arbitrary[IntTree.Tree] = Arbitrary(for {
    values <- Gen.someOf(0 to 20)
  } yield values.foldLeft(IntTree.empty)(IntTree.insert))

}

class IntRangeTests extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  import test.kofre.IntRangeGenerator._

  "insert one" in forAll { (it1: IntTree.Tree) =>
    println(IntTree.show(it1))
    for (x <- 0 to 20) assert(IntTree.contains(IntTree.insert(it1, x), x))
  }

  implicit val shortlists: Arbitrary[Seq[Int]] = Arbitrary(Gen.someOf(0 to 20))

  "insert all" in forAll { (list: Seq[Int]) =>
    val range = list.foldLeft(IntTree.empty)(IntTree.insert)
    println(list)
    println(IntTree.show(range))
    list.foreach { i =>
      assert(IntTree.contains(range, i))
    }
  }

}

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
