package tests.rescala.static.signals

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import tests.rescala.testtools.RETests
import rescala.extra.simpleprop.SimpleScheduler.SignalWithInvariants
import rescala.extra.simpleprop.SimpleStruct
import rescala.interface.RescalaInterface

class ErrorPropagation extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {
    val engine: RescalaInterface[SimpleStruct] = RescalaInterface.interfaceFor(rescala.extra.simpleprop.SimpleScheduler)
    import engine._

    "EXPERIMENTAL" in forAll(Gen.posNum[Int]) { (n: Int) =>
      val e = Evt[Int]()
      val s: Signal[Int] = e.count

      val t = s.changed.fold(Seq.empty[Int]) { (acc, c) => acc :+ c }

      s.specify(Seq(
        a => a < 3
      ))

//      TestSpecification {
//        if (s() < 3) {
//          assert(s() == t().size)
//        } else {
//          assert(s() > t().size)
//        }
//      }

      1 to n foreach { i => e.fire(i) }
    }
}
