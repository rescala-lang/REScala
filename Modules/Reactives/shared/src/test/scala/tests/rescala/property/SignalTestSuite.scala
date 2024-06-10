package tests.rescala.property

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import reactives.core.infiltration.Infiltrator

import scala.collection.Seq
import scala.collection.mutable.ListBuffer
import scala.util.Random

class SignalTestSuite extends munit.ScalaCheckSuite {
  val ie = new Infiltrator()
  import ie.api.*
  import ie.assertLevel

  implicit val shortlists: Arbitrary[Seq[Int]]  = Arbitrary(Gen.someOf(0 to 1000))
  implicit val positiveIntegers: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

  property("get last n signals") {
    forAll { (fireValues: Seq[Int], lastN: Int) =>
      val e                                          = Evt[Int]()
      val s: Signal[scala.collection.LinearSeq[Int]] = e.list(lastN)

      var fireCount = 0
      val fire = (value: Int) => {
        e.fire(value);
        fireCount += 1
      }

      // make sure that the the list is initial empty
      assert(s.readValueOnce.isEmpty)
      for i <- fireValues.indices do {
        fire(fireValues(i))
        // make sure that the list of the last n events contains at most `lastN` events and exactly `fireCount` if it is less than `lastN`
        assert(s.readValueOnce.length == Math.min(fireCount, lastN))
        // make sure that the elements of the list are as expected
        val expectedLastN = fireValues.take(fireCount).takeRight(lastN)
        assert(expectedLastN == s.readValueOnce)
      }
      // make sure that the event was fired for every item in `fireValues`
      assert(fireCount == fireValues.length)
    }
  }

  implicit val signalsGen: Arbitrary[List[Signal[Int]]] = Arbitrary(
    for
      i <- Gen.oneOf(0 to 1000)
    yield {
      val root    = Var(0)
      val signals = new ListBuffer[Signal[Int]]()
      signals += root
      0 to i foreach { _ =>
        val randomSignal = signals(Random.nextInt(signals.length))
        signals += Signal { 1 + randomSignal.value }
      }
      signals.toList
    }
  )

  test("level Is Correctly Computed") {
    forAll { (signals: List[Signal[Int]]) =>
      for signal <- signals do {
        assertLevel(signal, signal.readValueOnce)
      }
    }
  }

  test("count Is Correctly Computed") {
    forAll(Gen.posNum[Int]) { (n: Int) =>
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()

      var count = 0
      s observe (c => {
        assert(c == count)
        count += 1
      })

      assert(s.now == 0)
      1 to n foreach { i => e.fire(i) }
      assert(s.now == n)
    }
  }

  test("latestOption Is Correctly Computed") {
    forAll(Gen.posNum[Int]) { (n: Int) =>
      val e                      = Evt[Int]()
      val s: Signal[Option[Int]] = e.holdOption()

      var latest: Option[Int] = None
      s.observe { lo =>
        assert(lo == latest); ()
      }

      assert(s.readValueOnce.isEmpty)
      1 to n foreach { i =>
        latest = Option(i)
        e.fire(i)
      }
    }
  }

  test("iterate only depends on init value") {
    forAll(Arbitrary.arbitrary[Array[Int]], Arbitrary.arbInt.arbitrary) {
      (values: Array[Int], initial: Int) =>
        var t   = 0;
        val evt = Evt[Int]()
        val func = (x: Int) => {
          t = x; x + 1
        }
        val sig = evt.iterate(initial)(func)

        values.indices foreach {
          i =>
            evt.fire(values(i))
            assertEquals((initial + i), t)
            assertEquals((initial + 1 + i), sig.now)
        }
    }
  }

}
