package tests.rescala.property

import org.scalacheck.Prop.*
import reactives.default.*
import tests.rescala.testtools.FunSuiteInvertedAssert

class filter_EventTest extends munit.ScalaCheckSuite {

  property("filter Is Correctly Applied") {
    forAll { (nums: List[Int]) =>
      val e = Evt[Int]()
      val f = e filter { _ % 2 == 0 }
      val s = f.list()

      nums foreach { num => e.fire(num) }

      val evenNums = nums filter { _ % 2 == 0 }
      assert(s.readValueOnce == evenNums.reverse)
    }

  }
}
