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

//    "EXPERIMENTAL" in forAll(Gen.posNum[Int]) { (n: Int) =>
//      val e = Evt[Int]()
//      val s: Signal[Int] = e.count()
//
//      val t = s.changed.fold(Seq.empty[Int]) { (acc, c) => acc :+ c }
//
//      s.specify(
//        Invariant { a => a < n }
//      )
//
//      1 to n foreach { i => e.fire(i) }
//    }

    "test single node" - {
      val v = Var(0)
      val s1 = Signal { v() * 2}
      val s2 = Signal { v() * 2}
      val s3 = Signal { s1() + s2()}

      v.setValueGenerator(Gen.choose(-3, 5))// (Arbitrary.arbitrary[Int])
      s1.setValueGenerator(Gen.choose(-10, 10)) // (Arbitrary.arbitrary[Int])
      s2.setValueGenerator(Gen.choose(5, 10)) // (Arbitrary.arbitrary[Int])

      s1.specify(
        Invariant {a => a >= 0}
      )
      s2.specify(
        Invariant {a => a >= 0}
      )
      s3.specify(
        Invariant {a => a >= 0}
      )

      s3.test()
    }
}
