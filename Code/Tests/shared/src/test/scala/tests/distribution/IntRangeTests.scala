package tests.distribution

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.dotstores.IntTree

object IntRangeGenerator {
  implicit val genDot: Arbitrary[IntTree.Tree] = Arbitrary(for {
    values <- Gen.someOf(0 to 20)
  } yield values.foldLeft(IntTree.empty)(IntTree.insert))

}


class IntRangeTests extends FreeSpec with ScalaCheckDrivenPropertyChecks {

  import IntRangeGenerator._

  "insert one" in forAll { (it1: IntTree.Tree) =>
    println(IntTree.show(it1))
    for (x <- 0 to 20) assert(IntTree.contains(IntTree.insert(it1, x), x))
  }

  implicit val shortlists: Arbitrary[Seq[Int]] = Arbitrary(Gen.someOf(0 to 20))

  "insert all" in forAll{ list: Seq[Int] =>
    val range = list.foldLeft(IntTree.empty)(IntTree.insert)
    println(list)
    println(IntTree.show(range))
    list.foreach { i =>
      assert(IntTree.contains(range, i))
    }
  }


}
