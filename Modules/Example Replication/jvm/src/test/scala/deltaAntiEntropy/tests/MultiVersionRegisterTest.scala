package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Lattice
import rdts.datatypes.contextual.MultiVersionRegister
import replication.JsoniterCodecs.given

import scala.collection.mutable
import scala.util.Random

object MVRegisterGenerators {
  def genMVRegister[A: Lattice](implicit
      a: Arbitrary[A],
      cA: JsonValueCodec[A],
  ): Gen[AntiEntropyContainer[MultiVersionRegister[A]]] = for {
    values <- Gen.containerOf[List, A](a.arbitrary)
    nClear <- Gen.posNum[Short]
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[MultiVersionRegister[A]]("a", network, mutable.Buffer())

    val ops = Random.shuffle(values.indices ++ List.fill(nClear.toInt)(-1))

    ops.foldLeft(AntiEntropyContainer[MultiVersionRegister[A]](ae)) {
      case (r, -1) => r.clear()
      case (r, n)  => r.write(using r.replicaID)(values(n))
    }
  }

  implicit def arbMVRegister[A: Lattice](implicit
      a: Arbitrary[A],
      cA: JsonValueCodec[A],
  ): Arbitrary[AntiEntropyContainer[MultiVersionRegister[A]]] =
    Arbitrary(genMVRegister)
}

class MultiVersionRegisterTest extends munit.ScalaCheckSuite {
  import MVRegisterGenerators.*

  given Lattice[Int]                         = math.max
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  property("write") {
    forAll { (reg: AntiEntropyContainer[MultiVersionRegister[Int]], v: Int) =>
      val written = reg.write(using reg.replicaID)(v)

      assert(
        written.read == Set(v),
        s"Sequentially writing a value should result in a singleton set containing that value, but ${written.read} does not equal ${Set(v)}"
      )
    }
  }
  property("clear") {
    forAll { (reg: AntiEntropyContainer[MultiVersionRegister[Int]]) =>
      val cleared = reg.clear()

      assert(
        cleared.read.isEmpty,
        s"Clearing the register should result in an empty set, but ${cleared.read} is not empty"
      )
    }
  }
  property("concurrent write") {
    forAll { (vA: Int, vB: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[MultiVersionRegister[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[MultiVersionRegister[Int]]("b", network, mutable.Buffer("a"))

      val ra0 = AntiEntropyContainer[MultiVersionRegister[Int]](aea).write(using aea.uid)(vA)
      val rb0 = AntiEntropyContainer[MultiVersionRegister[Int]](aeb).write(using aeb.uid)(vB)

      AntiEntropy.sync(aea, aeb)

      val ra1 = ra0.processReceivedDeltas()
      val rb1 = rb0.processReceivedDeltas()

      assert(
        ra1.read == Set(vA, vB),
        s"Concurrently writing two values should result in a set containing both values, but ${ra1.read} does not equal ${Set(vA, vB)}"
      )
      assert(
        rb1.read == Set(vA, vB),
        s"Concurrently writing two values should result in a set containing both values, but ${rb1.read} does not equal ${Set(vA, vB)}"
      )
    }
  }
  property("concurrent write/clear") {
    forAll { (v: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[MultiVersionRegister[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[MultiVersionRegister[Int]]("b", network, mutable.Buffer("a"))

      val ra0 = AntiEntropyContainer[MultiVersionRegister[Int]](aea).write(using aea.uid)(v)
      val rb0 = AntiEntropyContainer[MultiVersionRegister[Int]](aeb).clear()

      AntiEntropy.sync(aea, aeb)

      val ra1 = ra0.processReceivedDeltas()
      val rb1 = rb0.processReceivedDeltas()

      assert(
        ra1.read == Set(v),
        s"Writing a value should win over a concurrent clear, but ${ra1.read} does not equal ${Set(v)}"
      )
      assert(
        rb1.read == Set(v),
        s"Writing a value should win over a concurrent clear, but ${rb1.read} does not equal ${Set(v)}"
      )
    }
  }
  property("convergence") {
    forAll {
      (valuesA: List[Int], nClearA: Short, valuesB: List[Int], nClearB: Short, networkGen: NetworkGenerator) =>
        val network = networkGen.make()
        val aea     = new AntiEntropy[MultiVersionRegister[Int]]("a", network, mutable.Buffer("b"))
        val aeb     = new AntiEntropy[MultiVersionRegister[Int]]("b", network, mutable.Buffer("a"))

        val opsA = Random.shuffle(valuesA.indices ++ List.fill(nClearA.toInt)(-1))
        val opsB = Random.shuffle(valuesB.indices ++ List.fill(nClearB.toInt)(-1))

        val ra0 = opsA.foldLeft(AntiEntropyContainer[MultiVersionRegister[Int]](aea)) {
          case (r, -1) => r.clear()
          case (r, n)  => r.write(using r.replicaID)(valuesA(n))
        }
        val rb0 = opsB.foldLeft(AntiEntropyContainer[MultiVersionRegister[Int]](aeb)) {
          case (r, -1) => r.clear()
          case (r, n)  => r.write(using r.replicaID)(valuesB(n))
        }

        AntiEntropy.sync(aea, aeb)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)

        val ra1 = ra0.processReceivedDeltas()
        val rb1 = rb0.processReceivedDeltas()

        assert(
          ra1.read == rb1.read,
          s"After synchronization messages were reliably exchanged all replicas should converge, but ${ra1.read} does not equal ${rb1.read}"
        )
    }
  }
}
