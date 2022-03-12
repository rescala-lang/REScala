package tests.distribution.delta.crdt.basic

import kofre.decompose.interfaces.PNCounterModule.{PNCounter, PNCounterSyntax}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.basic._
import rescala.extra.replication.AntiEntropy
import tests.distribution.delta.crdt.basic.NetworkGenerators._

import scala.collection.mutable

object PNCounterGenerator {
  val genPNCounter: Gen[BasicPNCounter] = for {
    nInc <- Gen.posNum[Int]
    nDec <- Gen.posNum[Int]
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[PNCounter]("a", network, mutable.Buffer())

    val inced = (0 to nInc).foldLeft(BasicPNCounter(ae)) {
      case (c, _) => c.inc()
    }

    (0 to nDec).foldLeft(inced) {
      case (c, _) => c.dec()
    }
  }

  implicit val arbPNCounter: Arbitrary[BasicPNCounter] = Arbitrary(genPNCounter)
}

class PNCounterTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import PNCounterGenerator._

  "inc" in forAll { counter: BasicPNCounter =>
    val inced = counter.inc()

    assert(
      inced.value == counter.value + 1,
      s"Incrementing the counter should increase its value by 1, but ${inced.value} does not equal ${counter.value} + 1"
    )
  }

  "dec" in forAll { counter: BasicPNCounter =>
    val deced = counter.dec()

    assert(
      deced.value == counter.value - 1,
      s"Decrementing the counter should decrease its value by 1, but ${deced.value} does not equal ${counter.value} - 1"
    )
  }

  "concurrent" in forAll { (incOrDecA: Boolean, incOrDecB: Boolean) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[PNCounter]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[PNCounter]("b", network, mutable.Buffer("a"))

    val ca0 = if (incOrDecA) BasicPNCounter(aea).inc() else BasicPNCounter(aea).dec()
    val cb0 = if (incOrDecB) BasicPNCounter(aeb).inc() else BasicPNCounter(aeb).dec()

    AntiEntropy.sync(aea, aeb)

    val ca1 = ca0.processReceivedDeltas()
    val cb1 = cb0.processReceivedDeltas()

    val sequential = if (incOrDecB) ca0.inc() else ca0.dec()

    assert(
      ca1.value == sequential.value,
      s"Concurrent execution of increment or decrement should be equivalent to any sequential execution, but ${ca1.value} does not equal ${sequential.value}"
    )

    assert(
      cb1.value == sequential.value,
      s"Concurrent execution of increment or decrement should be equivalent to any sequential execution, but ${cb1.value} does not equal ${sequential.value}"
    )
  }

  "convergence" in forAll { (incA: Short, decA: Short, incB: Short, decB: Short, network: Network) =>
    val aea = new AntiEntropy[PNCounter]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[PNCounter]("b", network, mutable.Buffer("a"))

    val incedA = (0 until incA.toInt).foldLeft(BasicPNCounter(aea)) {
      case (c, _) => c.inc()
    }
    val ca0 = (0 until decA.toInt).foldLeft(incedA) {
      case (c, _) => c.dec()
    }
    val incedB = (0 until incB.toInt).foldLeft(BasicPNCounter(aeb)) {
      case (c, _) => c.inc()
    }
    val cb0 = (0 until decB.toInt).foldLeft(incedB) {
      case (c, _) => c.dec()
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
