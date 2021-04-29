package tests.distribution.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.crdt.MVRegister
import rescala.extra.lattices.delta.crdt.MVRegister._
import rescala.extra.lattices.delta.{AntiEntropy, CContext, Network, UIJDLattice}
import tests.distribution.delta.NetworkGenerators.arbNetwork

import scala.collection.mutable
import scala.util.Random

object MVRegisterGenerators {
  def genMVRegister[A: UIJDLattice, C: CContext](implicit
      a: Arbitrary[A],
      cA: JsonValueCodec[A],
      cC: JsonValueCodec[C]
  ): Gen[MVRegister[A, C]] = for {
    values <- Gen.containerOf[List, A](a.arbitrary)
    nClear <- Gen.posNum[Short]
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[MVRegister.State[A, C]]("a", network, mutable.Buffer())

    val ops = Random.shuffle(values.indices ++ List.fill(nClear.toInt)(-1))

    ops.foldLeft(MVRegister(ae)) {
      case (r, -1) => r.clear()
      case (r, n)  => r.write(values(n))
    }
  }

  implicit def arbMVRegister[A: UIJDLattice, C: CContext](implicit
      a: Arbitrary[A],
      cA: JsonValueCodec[A],
      cC: JsonValueCodec[C]
  ): Arbitrary[MVRegister[A, C]] =
    Arbitrary(genMVRegister)
}

class MVRegisterTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import MVRegisterGenerators._

  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "write" in forAll { (reg: MVRegister[Int, DietMapCContext], v: Int) =>
    val written = reg.write(v)

    assert(
      written.read == Set(v),
      s"Sequentially writing a value should result in a singleton set containing that value, but ${written.read} does not equal ${Set(v)}"
    )
  }

  "clear" in forAll { reg: MVRegister[Int, DietMapCContext] =>
    val cleared = reg.clear()

    assert(
      cleared.read.isEmpty,
      s"Clearing the register should result in an empty set, but ${cleared.read} is not empty"
    )
  }

  "concurrent write" in forAll { (vA: Int, vB: Int) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[MVRegister.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[MVRegister.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

    val ra0 = MVRegister[Int, DietMapCContext](aea).write(vA)
    val rb0 = MVRegister[Int, DietMapCContext](aeb).write(vB)

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

  "concurrent write/clear" in forAll { v: Int =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[MVRegister.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[MVRegister.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

    val ra0 = MVRegister[Int, DietMapCContext](aea).write(v)
    val rb0 = MVRegister[Int, DietMapCContext](aeb).clear()

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

  "convergence" in forAll {
    (valuesA: List[Int], nClearA: Short, valuesB: List[Int], nClearB: Short, network: Network) =>
      val aea = new AntiEntropy[MVRegister.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[MVRegister.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

      val opsA = Random.shuffle(valuesA.indices ++ List.fill(nClearA.toInt)(-1))
      val opsB = Random.shuffle(valuesB.indices ++ List.fill(nClearB.toInt)(-1))

      val ra0 = opsA.foldLeft(MVRegister(aea)) {
        case (r, -1) => r.clear()
        case (r, n)  => r.write(valuesA(n))
      }
      val rb0 = opsB.foldLeft(MVRegister(aeb)) {
        case (r, -1) => r.clear()
        case (r, n)  => r.write(valuesB(n))
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
