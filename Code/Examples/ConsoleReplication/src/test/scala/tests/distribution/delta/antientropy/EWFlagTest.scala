package tests.distribution.delta.antientropy

import org.scalacheck.{Arbitrary, Gen}
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.replication.AntiEntropy
import kofre.decompose.containers.{AntiEntropyCRDT, Network}
import NetworkGenerators._
import kofre.datatypes.EnableWinsFlag
import org.scalacheck.Prop._

import scala.collection.mutable
import scala.util.Random

object EWFlagGenerators {
  def genEWFlag: Gen[AntiEntropyCRDT[EnableWinsFlag]] = for {
    nEnable  <- Gen.posNum[Int]
    nDisable <- Gen.posNum[Int]
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer())

    val ops = Random.shuffle(List.fill(nEnable)(1) ++ List.fill(nDisable)(0))

    ops.foldLeft(AntiEntropyCRDT[EnableWinsFlag](ae)) {
      case (f, 0) => f.disable()
      case (f, 1) => f.enable()
      // default case is only needed to stop the compiler from complaining about non-exhaustive match
      case (f, _) => f
    }
  }

  implicit def arbEWFlag: Arbitrary[AntiEntropyCRDT[EnableWinsFlag]] = Arbitrary(genEWFlag)
}

class EWFlagTest extends munit.ScalaCheckSuite {
  import EWFlagGenerators._

  property("enable") {
    forAll { (flag: AntiEntropyCRDT[EnableWinsFlag]) =>
      val flagEnabled = flag.enable()

      assert(
        flagEnabled.read,
        s"After enabling the flag it should read true, but $flagEnabled.read returns false"
      )
    }
  }

  property("disable") {
    forAll { (flag: AntiEntropyCRDT[EnableWinsFlag]) =>
      val flagDisabled = flag.disable()

      assert(
        !flagDisabled.read,
        s"After disabling the flag it should read false, but $flagDisabled.false returns true"
      )
    }
  }

  test("concurrent enable") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

    val fa0 = AntiEntropyCRDT[EnableWinsFlag](aea).enable()
    val fb0 = AntiEntropyCRDT[EnableWinsFlag](aeb).enable()

    AntiEntropy.sync(aea, aeb)

    val fa1 = fa0.processReceivedDeltas()
    val fb1 = fb0.processReceivedDeltas()

    assert(
      fa1.read,
      s"Concurrent enable should have the same effect as sequential enable, but $fa1.read returns false"
    )
    assert(
      fb1.read,
      s"Concurrent enable should have the same effect as sequential enable, but $fb1.read returns false"
    )
  }

  test("concurrent disable") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

    val fa0 = AntiEntropyCRDT[EnableWinsFlag](aea)
    val fb0 = AntiEntropyCRDT[EnableWinsFlag](aeb)

    val fa1 = fa0.disable()
    val fb1 = fb0.disable()

    AntiEntropy.sync(aea, aeb)

    val fa2 = fa1.processReceivedDeltas()
    val fb2 = fb1.processReceivedDeltas()

    assert(
      !fa2.read,
      s"Concurrent disable should have the same effect as sequential disable, but $fa2.read returns true"
    )
    assert(
      !fb2.read,
      s"Concurrent disable should have the same effect as sequential disable, but $fb2.read returns true"
    )
  }

  test("concurrent enable/disable") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

    val fa0 = AntiEntropyCRDT[EnableWinsFlag](aea).enable()
    val fb0 = AntiEntropyCRDT[EnableWinsFlag](aeb).disable()

    AntiEntropy.sync(aea, aeb)

    val fa1 = fa0.processReceivedDeltas()
    val fb1 = fb0.processReceivedDeltas()

    assert(
      fa1.read,
      s"Enable should win over concurrent disable, but $fa1.read returns false"
    )
    assert(
      fb1.read,
      s"Enable should win over concurrent disable, but $fb1.read returns false"
    )
  }

  property("convergence") {
    forAll { (enableA: Short, disableA: Short, enableB: Short, disableB: Short, network: Network) =>
      val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

      val opsA = Random.shuffle(List.fill(enableA.toInt)(1) ++ List.fill(disableA.toInt)(0))
      val opsB = Random.shuffle(List.fill(enableB.toInt)(1) ++ List.fill(disableB.toInt)(0))

      val fa0 = opsA.foldLeft(AntiEntropyCRDT[EnableWinsFlag](aea)) {
        case (f, 0) => f.disable()
        case (f, 1) => f.enable()
        // default case is only needed to stop the compiler from complaining about non-exhaustive match
        case (f, _) => f
      }
      val fb0 = opsB.foldLeft(AntiEntropyCRDT[EnableWinsFlag](aeb)) {
        case (f, 0) => f.disable()
        case (f, 1) => f.enable()
        // default case is only needed to stop the compiler from complaining about non-exhaustive match
        case (f, _) => f
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val fa1 = fa0.processReceivedDeltas()
      val fb1 = fb0.processReceivedDeltas()

      assert(
        fa1.read == fb1.read,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${fa1.read} does not equal ${fb1.read}"
      )
    }
  }
}
