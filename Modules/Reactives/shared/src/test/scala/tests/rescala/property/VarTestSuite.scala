package tests.rescala.property

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import tests.rescala.testtools.RETests
import org.scalacheck.Prop.*
import reactives.default.*

class VarTestSuite extends munit.ScalaCheckSuite {

  property("get Val After Creation Returns Initialization Value") {
    forAll { (initialValue: Int) =>
      val v = Var(initialValue)
      assert(v.readValueOnce == initialValue)
    }
  }

  property("changed is correctly computed") {
    forAll(Gen.containerOf[List, Int](Gen.posNum[Int])) { (list: List[Int]) =>
      val v = Var(-1)
      val e = v.changed
      val s = e.hold()

      list.foreach { n =>
        v.set(n)
        assertEquals((n), s.now)
      }
    }
  }
}
