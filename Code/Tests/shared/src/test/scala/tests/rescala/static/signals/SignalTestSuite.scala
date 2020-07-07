package tests.rescala.static.signals

import java.util.concurrent.atomic.AtomicInteger

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.core.infiltration.Infiltrator.assertLevel
import tests.rescala.testtools.RETests

import scala.collection.Seq
import scala.collection.mutable.ListBuffer
import scala.util.Random


class SignalTestSuite extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {
  multiEngined { engine => import engine._


  test("handler Is Called When Change Occurs"){

    var test = 0
    val v1 = Var(1)
    val v2 = Var(2)

    val s1 = Signals.lift(v1, v2) { _ + _ }
    s1.changed += { (_) => test += 1 }

    assert(s1.readValueOnce == 3)
    assert(test == 0)

    v2.set(3)
    assert(s1.readValueOnce == 4)
    assert(test == 1)

    v2.set(3)
    assert(s1.readValueOnce == 4)
    assert(test == 1)

  }


  test("signal Reevaluates The Expression When Something It Depends On Is Updated"){
    val v = Var(0)
    var i = 1
    val s = Signal { v() + i }
    i = 2
    assert(s.readValueOnce == 1)
    v.set(2)
    assert(s.readValueOnce == 4)
  }

  test("the Expression Is Not Evaluated Every Time now Is Called"){
    var a = 10
    val s = Signal(1 + 1 + a)
    assert(s.readValueOnce === 12)
    a = 11
    assert(s.readValueOnce === 12)
  }



  test("level Is Correctly Computed"){

    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    assertLevel(v, 0)
    assertLevel(s1, 1)
    assertLevel(s2, 1)
    assertLevel(s3, 2)


  }



  test("dependant Is Only Invoked On Value Changes"){
    var changes = 0
    val v = Var(1)
    val s = Signal {
      changes += 1; v() + 1
    }
    assert(changes === 1)
    assert(s.readValueOnce === 2)
    v.set(2)
    assert(s.readValueOnce === 3)
    assert(changes === 2)
    v.set(2)
    assert(changes === 2) // is actually 3
  }







  test("creating signals in signals based on changing signals"){
    val v0 = Var("level 0")
    val v3 = v0.map(_ + "level 1").map(_  + "level 2").map(_ + "level 3")

    val `dynamic signal changing from level 1 to level 5` = Signal {
      if (v0() == "level 0") v0() else {
        v3.map(_ + "level 4 inner").apply()
      }
    }
    assert(`dynamic signal changing from level 1 to level 5`.readValueOnce == "level 0")
    //note: will start with level 5 because of static guess of current level done by the macro expansion
    assertLevel(`dynamic signal changing from level 1 to level 5`, 5)

    v0.set("level0+")
    assert(`dynamic signal changing from level 1 to level 5`.readValueOnce == "level0+level 1level 2level 3level 4 inner")
    assertLevel(`dynamic signal changing from level 1 to level 5`, 5)
  }



  test("signal Reevaluates The Expression"){
    val v = Var(0)
    var i = 1
    val s: Signal[Int] = v.map { _ => i }
    i = 2
    v.set(2)
    assert(s.readValueOnce == 2)
  }

  test("the Expression Is Note Evaluated Every Time Get Val Is Called"){
    var a = 10
    val s: Signal[Int] = Signals.static()(_ => 1 + 1 + a)
    assert(s.readValueOnce === 12)
    a = 11
    assert(s.readValueOnce === 12)
  }


  test("simple Signal Returns Correct Expressions"){
    val s: Signal[Int] = Signals.static()(_ => 1 + 1 + 1)
    assert(s.readValueOnce === 3)
  }

  test("the Expression Is Evaluated Only Once"){

    var a = 0
    val v = Var(10)
    val s1: Signal[Int] = v.map { i =>
      a += 1
      i % 10
    }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
    assert(s1.readValueOnce === 1)
  }

  test("handlers Are Executed"){

    val test = new AtomicInteger(0)
    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1, s2) { _ + _ }

    s1.changed += { (_) => test.incrementAndGet() }
    s2.changed += { (_) => test.incrementAndGet() }
    s3.changed += { (_) => test.incrementAndGet() }

    assert(test.get == 0)

    v.set(3)
    assert(test.get == 3)
  }

  test("level Is Correctly Computed with combinators"){

    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1, s2) { _ + _ }

    assertLevel(v, 0)
    assertLevel(s1, 1)
    assertLevel(s2, 1)
    assertLevel(s3, 2)
  }


  test("no Change Propagation"){
    val v = Var(1)
    val s = v.map(_ => 1)
    val s2 = Signal { s() }

    assert(s2.readValueOnce === 1)
    assert(s.readValueOnce === 1)

    v.set(2)
    assert(s.readValueOnce === 1)
    assert(s2.readValueOnce === 1)


    v.set(2)
    assert(s2.readValueOnce === 1)
    assert(s.readValueOnce === 1)


    v.set(3)
    assert(s2.readValueOnce === 1)
    assert(s.readValueOnce === 1)


  }

  test("graph cost example") {

    def mini(x: Var[Map[Signal[Int], Int]]): Signal[Int] = Signal.dynamic {
      val (node, value) = x().minBy  { case (n, v) => n() + v }
      node() + value
    }

    val root = Signal {0}

    val parentA = Var(Map(root -> 2))
    val WeightA = mini(parentA)

    assert(WeightA.readValueOnce == 2)

    val parentB = Var(Map(root -> 1))
    val WeightB = mini(parentB)

    assert(WeightB.readValueOnce == 1)


    val parentC = Var(Map(WeightA -> 3, WeightB -> 10))
    val WeightC = mini(parentC)

    assert(WeightC.readValueOnce == 5)


    parentC.transform(_ + (WeightB -> 1))

    assert(WeightC.readValueOnce == 2)

  }

  "property based tests" - {
    implicit val shortlists: Arbitrary[Seq[Int]] = Arbitrary(Gen.someOf(0 to 1000))
    implicit val positiveIntegers: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

    "get last n signals" in forAll { (fireValues: Seq[Int], lastN: Int) =>
      val e = Evt[Int]()
      val s: Signal[scala.collection.LinearSeq[Int]] = e.last(lastN)

      var fireCount = 0
      val fire = (value: Int) => {
        e.fire(value);
        fireCount += 1
      }

      // make sure that the the list is initial empty
      assert(s.readValueOnce.isEmpty)
      for (i <- fireValues.indices) {
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


    implicit val signalsGen: Arbitrary[List[Signal[Int]]] = Arbitrary(
      for {
        i <- Gen.oneOf(0 to 1000)
      }
      yield {
        val root = Var(0)
        var signals = new ListBuffer[Signal[Int]]()
        signals += root
        0 to i foreach { _ =>
          val randomSignal = signals(Random.nextInt(signals.length))
          signals += Signal{ 1 + randomSignal() }
        }
        signals.toList
      }
    )

    "level Is Correctly Computed" in forAll { (signals: List[Signal[Int]]) =>
      for (signal <- signals) {
        assertLevel(signal, signal.readValueOnce)
      }
    }

    "count Is Correctly Computed" in forAll(Gen.posNum[Int]) { (n: Int) =>
      val e = Evt[Int]()
      val s: Signal[Int] = e.count

      var count = 0
      s observe(c => {
        assert(c == count)
        count += 1
      })

      assert(s.now == 0)
      1 to n foreach { i => e.fire(i) }
      assert(s.now == n)
    }

    "latestOption Is Correctly Computed" in forAll(Gen.posNum[Int]) { (n: Int) =>
      val e = Evt[Int]()
      val s: Signal[Option[Int]] = e.latestOption()

      var latest: Option[Int] = None
      s observe(lo =>
        assert(lo == latest)
        )

      assert(s.readValueOnce.isEmpty)
      1 to n foreach { i =>
        latest = Option(i)
        e.fire(i)
      }
    }

    "iterate only depends on init value" in forAll(Arbitrary.arbitrary[Array[Int]], Arbitrary.arbInt.arbitrary) {
      (values: Array[Int], initial: Int) =>
        var t = 0;
        val evt = Evt[Int]
        val func = (x: Int) => {
          t = x; x + 1
        }
        val sig = evt.iterate(initial)(func)

        values.indices foreach {
          i =>
            evt.fire(values(i))
            t should be (initial + i)
            sig.now should be (initial + 1 + i)
        }

    }
  }
} }
