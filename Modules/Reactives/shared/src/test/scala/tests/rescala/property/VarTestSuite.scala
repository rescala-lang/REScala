package tests.rescala.property

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import tests.rescala.testtools.RETests

class VarTestSuite extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {
  multiEngined { engine =>
    import engine._

    "get Val After Creation Returns Initialization Value" in forAll { (initialValue: Int) =>
      val v = Var(initialValue)
      assert(v.readValueOnce == initialValue)
    }

    "changed is correctly computed" in forAll(Gen.containerOf[List, Int](Gen.posNum[Int])) { (list: List[Int]) =>
      val v = Var(-1)
      val e = v.changed
      val s = e.hold()

      list.foreach { n =>
        v.set(n)
        s.now should be(n)
      }
    }
  }
}
