package deltaAntiEntropy.tests

import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import rdts.datatypes.PosNegCounter
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.*

import scala.collection.mutable

object PosNegCounterGenerator {
  val genPosNegCounter: Gen[AntiEntropyContainer[PosNegCounter]] = for {
    nInc <- Gen.posNum[Int]
    nDec <- Gen.posNum[Int]
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[PosNegCounter]("a", network, mutable.Buffer())

    val inced = (0 to nInc).foldLeft(AntiEntropyContainer(ae)) {
      case (c, _) => c.inc()(using c.replicaID)
    }

    (0 to nDec).foldLeft(inced) {
      case (c, _) => c.dec()(using c.replicaID)
    }
  }

  implicit val arbPosNegCounter: Arbitrary[AntiEntropyContainer[PosNegCounter]] = Arbitrary(genPosNegCounter)
}

class PosNegCounterTest extends munit.ScalaCheckSuite {
  import PosNegCounterGenerator.*
  property("inc") {
    forAll { (counter: AntiEntropyContainer[PosNegCounter]) =>
      val before = counter.value
      val inced  = counter.inc()(using counter.replicaID)

      assert(
        inced.value == before + 1,
        s"Incrementing the counter should increase its value by 1, but ${inced.value} does not equal ${counter.value} + 1"
      )
    }
  }
  property("dec") {
    forAll { (counter: AntiEntropyContainer[PosNegCounter]) =>
      val before = counter.value
      val deced  = counter.dec()(using counter.replicaID)

      assert(
        deced.value == before - 1,
        s"Decrementing the counter should decrease its value by 1, but ${deced.value} does not equal ${counter.value} - 1"
      )
    }
  }
  property("concurrent") {
    forAll { (incOrDecA: Boolean, incOrDecB: Boolean) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[PosNegCounter]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[PosNegCounter]("b", network, mutable.Buffer("a"))
      val aec = new AntiEntropy[PosNegCounter]("c", network, mutable.Buffer("c"))

      val ca0 = if (incOrDecA) AntiEntropyContainer[PosNegCounter](aea).inc()(using aea.uid)
      else AntiEntropyContainer[PosNegCounter](aea).dec()(using aea.uid)
      val cb0 = if (incOrDecB) AntiEntropyContainer[PosNegCounter](aeb).inc()(using aeb.uid)
      else AntiEntropyContainer[PosNegCounter](aeb).dec()(using aeb.uid)

      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      val sequential = AntiEntropyContainer(aec)
      if (incOrDecA) sequential.inc()(using sequential.replicaID) else sequential.dec()(using sequential.replicaID)
      if (incOrDecB) sequential.inc()(using sequential.replicaID) else sequential.dec()(using sequential.replicaID)

      assert(
        ca1.value == sequential.value,
        s"Concurrent execution of increment or decrement should be equivalent to any sequential execution, but ${ca1.value} does not equal ${sequential.value}"
      )

      assert(
        cb1.value == sequential.value,
        s"Concurrent execution of increment or decrement should be equivalent to any sequential execution, but ${cb1.value} does not equal ${sequential.value}"
      )
    }
  }
  property("convergence") {
    forAll { (incA: Byte, decA: Byte, incB: Byte, decB: Byte, networkGen: NetworkGenerator) =>
      val network = networkGen.make()
      val aea     = new AntiEntropy[PosNegCounter]("a", network, mutable.Buffer("b"))
      val aeb     = new AntiEntropy[PosNegCounter]("b", network, mutable.Buffer("a"))

      val incedA = (0 until incA.toInt).foldLeft(AntiEntropyContainer[PosNegCounter](aea)) {
        case (c, _) => c.inc()(using c.replicaID)
      }
      val ca0 = (0 until decA.toInt).foldLeft(incedA) {
        case (c, _) => c.dec()(using c.replicaID)
      }
      val incedB = (0 until incB.toInt).foldLeft(AntiEntropyContainer[PosNegCounter](aeb)) {
        case (c, _) => c.inc()(using c.replicaID)
      }
      val cb0 = (0 until decB.toInt).foldLeft(incedB) {
        case (c, _) => c.dec()(using c.replicaID)
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      assert(
        ca1.value == cb1.value,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${ca1.value} does not equal ${cb1.value}"
      )
    }
  }
}
