package deltaAntiEntropy.tests

import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.datatypes.contextual.EnableWinsFlag
import replication.JsoniterCodecs.given

import scala.collection.mutable
import scala.util.Random

object EWFlagGenerators {
  def genEWFlag: Gen[AntiEntropyContainer[EnableWinsFlag]] =
    for
      nEnable  <- Gen.posNum[Int]
      nDisable <- Gen.posNum[Int]
    yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer())

      val ops = Random.shuffle(List.fill(nEnable)(1) ++ List.fill(nDisable)(0))

      ops.foldLeft(AntiEntropyContainer[EnableWinsFlag](ae)) {
        case (f, 0) => f.mod(_.disable())
        case (f, 1) => f.mod(_.enable(using rdts.base.Uid.predefined(ae.replicaID))())
        // default case is only needed to stop the compiler from complaining about non-exhaustive match
        case (f, _) => f
      }
    }

  implicit def arbEWFlag: Arbitrary[AntiEntropyContainer[EnableWinsFlag]] = Arbitrary(genEWFlag)
}

class EWFlagTest extends munit.ScalaCheckSuite {
  import EWFlagGenerators.*

  property("enable") {
    forAll { (flag: AntiEntropyContainer[EnableWinsFlag]) =>
      val flagEnabled = flag.mod(_.enable(using flag.replicaID)())

      assert(
        flagEnabled.data.read,
        s"After enabling the flag it should read true, but $flagEnabled.read returns false"
      )
    }
  }

  property("disable") {
    forAll { (flag: AntiEntropyContainer[EnableWinsFlag]) =>
      val flagDisabled = flag.mod(_.disable())

      assert(
        !flagDisabled.data.read,
        s"After disabling the flag it should read false, but $flagDisabled.false returns true"
      )
    }
  }

  test("concurrent enable") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

    val fa0 = AntiEntropyContainer[EnableWinsFlag](aea).mod(_.enable(using aea.uid)())
    val fb0 = AntiEntropyContainer[EnableWinsFlag](aeb).mod(_.enable(using aeb.uid)())

    AntiEntropy.sync(aea, aeb)

    val fa1 = fa0.processReceivedDeltas()
    val fb1 = fb0.processReceivedDeltas()

    assert(
      fa1.data.read,
      s"Concurrent enable should have the same effect as sequential enable, but $fa1.data.read returns false"
    )
    assert(
      fb1.data.read,
      s"Concurrent enable should have the same effect as sequential enable, but $fb1.data.read returns false"
    )
  }

  test("concurrent disable") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

    val fa0 = AntiEntropyContainer[EnableWinsFlag](aea)
    val fb0 = AntiEntropyContainer[EnableWinsFlag](aeb)

    val fa1 = fa0.mod(_.disable())
    val fb1 = fb0.mod(_.disable())

    AntiEntropy.sync(aea, aeb)

    val fa2 = fa1.processReceivedDeltas()
    val fb2 = fb1.processReceivedDeltas()

    assert(
      !fa2.data.read,
      s"Concurrent disable should have the same effect as sequential disable, but $fa2.data.read returns true"
    )
    assert(
      !fb2.data.read,
      s"Concurrent disable should have the same effect as sequential disable, but $fb2.read returns true"
    )
  }

  test("concurrent enable/disable") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

    val fa0 = AntiEntropyContainer[EnableWinsFlag](aea).mod(_.enable(using aea.uid)())
    val fb0 = AntiEntropyContainer[EnableWinsFlag](aeb).mod(_.disable())

    AntiEntropy.sync(aea, aeb)

    val fa1 = fa0.processReceivedDeltas()
    val fb1 = fb0.processReceivedDeltas()

    assert(
      fa1.data.read,
      s"Enable should win over concurrent disable, but $fa1.data.read returns false"
    )
    assert(
      fb1.data.read,
      s"Enable should win over concurrent disable, but $fb1.data.read returns false"
    )
  }

  property("convergence") {
    forAll { (enableA: Short, opsA: List[Boolean], opsB: List[Boolean], networkGen: NetworkGenerator) =>
      val network = networkGen.make()
      val aea     = new AntiEntropy[EnableWinsFlag]("a", network, mutable.Buffer("b"))
      val aeb     = new AntiEntropy[EnableWinsFlag]("b", network, mutable.Buffer("a"))

      val fa0 = opsA.foldLeft(AntiEntropyContainer[EnableWinsFlag](aea)) {
        case (f, false) => f.mod(_.disable())
        case (f, true)  => f.mod(_.enable(using f.replicaID)())
      }
      val fb0 = opsB.foldLeft(AntiEntropyContainer[EnableWinsFlag](aeb)) {
        case (f, false) => f.mod(_.disable())
        case (f, true)  => f.mod(_.enable(using f.replicaID)())
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val fa1 = fa0.processReceivedDeltas()
      val fb1 = fb0.processReceivedDeltas()

      assert(
        fa1.data.read == fb1.data.read,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${fa1.data.read} does not equal ${fb1.data.read}"
      )
    }
  }
}
