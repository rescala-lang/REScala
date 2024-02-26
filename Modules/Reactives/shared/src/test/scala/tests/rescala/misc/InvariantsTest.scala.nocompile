package tests.rescala.misc

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import reactives.extra.invariant.{Invariant, InvariantApi}
import tests.rescala.testtools.RETests

class InvariantsTest extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {

  import reactives.extra.invariant.InvariantApi.*
  val sched = InvariantApi.scheduler
  import sched.*

  "expect invalid invariants to fail" in forAll(Gen.posNum[Int]) { (n: Int) =>
    assertThrows[InvariantViolationException] {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()

      s.specify(
        Invariant[Int] { a => a < n }
      )

      1 to n foreach { i => e.fire(i) }
    }
  }

  "correct invariants do not fail" in {
    val v = Var(0)
    val s1 = Signal {
      v.value * 2
    }
    val s2 = Signal {
      v.value * 2
    }
    val s3 = Signal {
      s1.value + s2.value
    }

    s1.setValueGenerator(Gen.choose(0, 10))
    s2.setValueGenerator(Gen.choose(0, 10))

    s3.specify(
      Invariant { a => a >= 0 }
    )

    s3.test()
  }

  "changes are propagated when testing invariants" in {
    val v1 = Var(0)
    val v2 = Var(0)

    val s1 = Signal {
      v1.value * 2
    }
    val s2 = Signal {
      v2.value * 2
    }

    // signal under test
    val sut = Signal {
      s1.value + s2.value
    }

    v1.setValueGenerator(Arbitrary.arbitrary[Int])
    v2.setValueGenerator(Arbitrary.arbitrary[Int])
    sut.specify(
      Invariant { value => value == (2 * (v1.now + v2.now)) }
    )

    sut.test()
  }

  "only closest generators are used" in {
    val top   = Var(10)
    val left  = Signal { top.value + 1 }
    val right = Signal { top.value + 2 }
    val sut   = Signal { left.value + right.value }

    val topChangedCount = top.changed.count()

    top.setValueGenerator(Arbitrary.arbitrary[Int])
    left.setValueGenerator(Arbitrary.arbitrary[Int])
    right.setValueGenerator(Arbitrary.arbitrary[Int])

    sut.test()

    assert(topChangedCount.now == 0)
  }

  "expect invalid invariants to fail when testing node" in {
    val v = Var("Hello")
    val sut = Signal {
      s"${v.value}, World!"
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
      s"${v.value}, World!"
    }

    v.setValueGenerator(Arbitrary.arbitrary[String])
    sut.specify(
      new Invariant("string_length", { value => value.length < 5 })
    )

    val caught = intercept[InvariantViolationException] { sut.test() }

    caught.getMessage.matches("$Value\\(.*\\) violates invariant string_length.*")

  }

  "signals can have multiple invariants" in {
    val a = Var(10)
    val b = Var(20)

    val sut = Signal { Math.pow(a.value.toDouble, 2) + Math.pow(b.value.toDouble, 2) }

    sut.specify(
      Invariant { value => value >= a.now },
      Invariant { value => value >= b.now }
    )

    a.setValueGenerator(Arbitrary.arbitrary[Int])
    b.setValueGenerator(Arbitrary.arbitrary[Int])

    sut.test()
  }

  "all invariants get tested" in forAll(Gen.choose(1, 50), Gen.choose(0, 49)) { (n: Int, failingIndex: Int) =>
    // Create an arbitrary amount of invariants and select one to fail
    whenever(failingIndex < n) {
      val invariants = 0 to n map { i =>
        new Invariant[Int](
          s"invariant$i",
          if (i == failingIndex) { value => value < 0 }
          else { value => value > 0 }
        )
      }

      val v   = Var(1)
      val sut = Signal { v.value }

      v.setValueGenerator(Gen.posNum[Int])
      sut.specify(invariants*)

      // expect the correct invariant to fail
      val caught = intercept[InvariantViolationException] { sut.test() }
      val regex  = "$Value\\(.*\\) violates invariant invariant" + failingIndex + ".*"
      caught.getMessage.matches(regex)

    }
  }

  "expect NoGeneratorException when calling test on untestable node" in {
    val v = Var(1)
    assertThrows[NoGeneratorException] { v.test() }
  }
}
