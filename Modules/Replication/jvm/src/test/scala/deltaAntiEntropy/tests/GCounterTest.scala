package deltaAntiEntropy.tests

import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.datatypes.GrowOnlyCounter
import replication.JsoniterCodecs.given

import scala.collection.mutable

object GCounterGenerators {
  val genGCounter: Gen[AntiEntropyContainer[GrowOnlyCounter]] =
    for
      n <- Gen.posNum[Int]
    yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[GrowOnlyCounter]("a", network, mutable.Buffer())

      (0 until n).foldLeft(AntiEntropyContainer[GrowOnlyCounter](ae)) {
        case (c, _) => c.map(_.inc())
      }
    }

  given arbGCounter: Arbitrary[AntiEntropyContainer[GrowOnlyCounter]] = Arbitrary(genGCounter)
}

class GCounterTest extends munit.ScalaCheckSuite {
  import GCounterGenerators.given

  extension (aec: AntiEntropyContainer[GrowOnlyCounter]) def value = aec.state.data.value

  property("inc") {
    forAll { (counter: AntiEntropyContainer[GrowOnlyCounter]) =>
      val before     = counter.state.data.value
      val counterInc = counter.map(_.inc())

      assertEquals(
        counterInc.state.data.value,
        before + 1,
        s"Incrementing the counter should increase its value by 1, but ${counterInc.state.data.value} does not equal ${counter.state.data.value} + 1"
      )
    }
  }

  test("concurrent inc") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[GrowOnlyCounter]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[GrowOnlyCounter]("b", network, mutable.Buffer("a"))

    val ca0 = AntiEntropyContainer[GrowOnlyCounter](aea).map(_.inc())
    val cb0 = AntiEntropyContainer[GrowOnlyCounter](aeb).map(_.inc())

    AntiEntropy.sync(aea, aeb)

    val ca1 = ca0.processReceivedDeltas()
    val cb1 = cb0.processReceivedDeltas()

    assertEquals(
      ca1.value,
      2,
      s"Concurrent increments should have the same behavior as sequential increments, but ${ca1.value} does not equal 2"
    )
    assertEquals(
      cb1.value,
      2,
      s"Concurrent increments should have the same behavior as sequential increments, but ${cb1.value} does not equal 2"
    )
  }

  property("convergence") {
    forAll { (incA: Short, incB: Short, networkGen: NetworkGenerator) =>
      val network = networkGen.make()
      val aea     = new AntiEntropy[GrowOnlyCounter]("a", network, mutable.Buffer("b"))
      val aeb     = new AntiEntropy[GrowOnlyCounter]("b", network, mutable.Buffer("a"))

      val ca0 = (0 until incA.toInt).foldLeft(AntiEntropyContainer[GrowOnlyCounter](aea)) {
        case (c, _) => c.map(_.inc())
      }
      val cb0 = (0 until incB.toInt).foldLeft(AntiEntropyContainer[GrowOnlyCounter](aeb)) {
        case (c, _) => c.map(_.inc())
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      assertEquals(
        ca1.value,
        cb1.value,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${ca1.value} does not equal ${cb1.value}"
      )
    }
  }
}
