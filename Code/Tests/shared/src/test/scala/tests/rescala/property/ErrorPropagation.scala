package tests.rescala.property

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.invariant.SimpleScheduler.SignalWithInvariants
import rescala.extra.invariant.{SimpleStruct, Invariant}
import rescala.interface.RescalaInterface
import tests.rescala.testtools.RETests

class ErrorPropagation extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {
    val engine: RescalaInterface[SimpleStruct] = RescalaInterface.interfaceFor(rescala.extra.invariant.SimpleScheduler)
    import engine._

    "EXPERIMENTAL" in forAll(Gen.posNum[Int]) { (n: Int) =>
      val e = Evt[Int]()
      val s: Signal[Int] = e.count()

      val t = s.changed.fold(Seq.empty[Int]) { (acc, c) => acc :+ c }

      s.specify(
        Invariant { a => a < n }
      )

      1 to n foreach { i => e.fire(i) }
    }

  /*
    "test single node" - {
      val v = Var(0)
      val s = Signal { v() * 2}

      s.specify(
        Invariant {a => a > 0}
      )

      s.setValueGenerator(Arbitrary.arbitrary[Int])

      s.test()
    }
   */
}
