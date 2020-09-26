package tests.rescala.property

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import tests.rescala.testtools.RETests

class filter_EventTest extends RETests with ScalaCheckDrivenPropertyChecks {
  multiEngined { engine =>
    import engine._

    "filter Is Correctly Applied" in forAll { (nums: List[Int]) =>
      val e = Evt[Int]()
      val f = e filter { _ % 2 == 0 }
      val s = f.list()

      nums foreach { num => e.fire(num) }

      val evenNums = nums filter { _ % 2 == 0 }
      assert(s.readValueOnce == evenNums.reverse)
    }

  }
}
