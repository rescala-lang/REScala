package tests.rescala.property

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.invariant.SimpleScheduler.SignalWithInvariants
import rescala.extra.invariant.{Invariant, InvariantViolationException, SimpleStruct}
import rescala.interface.RescalaInterface
import tests.rescala.testtools.RETests

class ErrorPropagation extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {
  val engine: RescalaInterface[SimpleStruct] = RescalaInterface.interfaceFor(rescala.extra.invariant.SimpleScheduler)

  import engine._

  "expect invalid invariants to fail" in forAll(Gen.posNum[Int]) { (n: Int) =>
    assertThrows[InvariantViolationException] {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()

      s.specify(
        Invariant { a => a < n }
      )

      1 to n foreach { i => e.fire(i) }
    }
  }

  "correct invariants do not fail" in {
    val v = Var(0)
    val s1 = Signal {
      v() * 2
    }
    val s2 = Signal {
      v() * 2
    }
    val s3 = Signal {
      s1() + s2()
    }

    s1.setValueGenerator(Gen.choose(0, 10)) // (Arbitrary.arbitrary[Int])
    s2.setValueGenerator(Gen.choose(0, 10)) // (Arbitrary.arbitrary[Int])

    s3.specify(
      Invariant { a => a >= 0 }
    )

    s3.test()
  }

  "changes are propagated when testing invariants" in {
    val v1 = Var(0)
    val v2 = Var(0)

    val s1 = Signal {
      v1() * 2
    }
    val s2 = Signal {
      v2() * 2
    }

    // signal under test
    val sut = Signal {
      s1() + s2()
    }

    v1.setValueGenerator(Arbitrary.arbitrary[Int])
    v2.setValueGenerator(Arbitrary.arbitrary[Int])
    sut.specify(
      Invariant { value => value == (2 * (v1.now + v2.now)) }
    )

    sut.test()
  }

  "expect invalid invariants to fail when testing node" in {
      val v = Var("Hello")
      val sut = Signal {
        s"${v()}, World!"
      }

      v.setValueGenerator(Arbitrary.arbitrary[String])
      sut.specify(
        Invariant { value => value.length < 5 }
      )

    assertThrows[InvariantViolationException] {
      sut.test()
    }
  }

  "invariants can have names" in {
    val v = Var("Hello")
    val sut = Signal {
      s"${v()}, World!"
    }

    v.setValueGenerator(Arbitrary.arbitrary[String])
    sut.specify(
      new Invariant("string_length", { value => value.length < 5 })
    )

    val caught = intercept[InvariantViolationException] {sut.test()}

    caught.getMessage.matches("$Value\\(.*\\) violates invariant string_length.*")

  }
}
