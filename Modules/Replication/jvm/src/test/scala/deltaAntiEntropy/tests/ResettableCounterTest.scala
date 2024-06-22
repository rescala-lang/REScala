package deltaAntiEntropy.tests

import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.datatypes.alternatives.ResettableCounter
import replication.JsoniterCodecs.given

import scala.collection.mutable
import scala.util.Random

object RCounterGenerators {
  def genRCounter: Gen[AntiEntropyContainer[ResettableCounter]] =
    for
      num <- Gen.choose(0, 100)
      ops <- Gen.listOfN(num, Gen.chooseNum(0, 3))
    yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[ResettableCounter]("a", network, mutable.Buffer())

      ops.foldLeft(AntiEntropyContainer[ResettableCounter](ae)) {
        case (c, 0) => c.mod(_.increment(using c.replicaID)())
        case (c, 1) => c.mod(_.decrement(using c.replicaID)())
        case (c, 2) => c.mod(_.reset())
        case (c, 3) => c.mod(_.fresh(using c.replicaID)())
        // default case is only needed to stop the compiler from complaining about non-exhaustive match
        case (c, _) => c
      }
    }

  implicit def arbRCounter: Arbitrary[AntiEntropyContainer[ResettableCounter]] = Arbitrary(genRCounter)
}

class ResettableCounterTest extends munit.ScalaCheckSuite {
  import RCounterGenerators.*

  property("increment") {
    forAll { (counter: AntiEntropyContainer[ResettableCounter]) =>
      val orig        = counter.data.value
      val incremented = counter.mod(_.increment(using counter.replicaID)())

      assert(
        incremented.data.value == orig + 1,
        s"Incrementing the counter should increase its value by 1, but ${incremented.data.value} does not equal ${counter.data.value} + 1"
      )
    }
  }

  property("decrement") {
    forAll { (counter: AntiEntropyContainer[ResettableCounter]) =>
      val orig        = counter.data.value
      val decremented = counter.mod(_.decrement(using counter.replicaID)())

      assert(
        decremented.data.value == orig - 1,
        s"Decrementing the counter should decrease its value by 1, but ${decremented.data.value} does not equal ${counter.data.value} - 1"
      )
    }
  }

  property("fresh") {
    forAll { (counter: AntiEntropyContainer[ResettableCounter]) =>
      val orig  = counter.data.value
      val fresh = counter.mod(_.fresh(using counter.replicaID)())

      assert(
        fresh.data.value == orig,
        s"Calling fresh should not change the value of the counter, but ${fresh.data.value} does not equal ${counter.data.value}"
      )
    }
  }

  property("reset") {
    forAll { (counter: AntiEntropyContainer[ResettableCounter]) =>
      val reset = counter.mod(_.reset())

      assertEquals(reset.data.value, 0, s"${reset.state}")
    }
  }

  property("concurrent increment/decrement/fresh") {
    forAll { (opA: Either[Unit, Boolean], opB: Either[Unit, Boolean]) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ResettableCounter]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ResettableCounter]("b", network, mutable.Buffer("a"))
      val aes = new AntiEntropy[ResettableCounter]("s", network, mutable.Buffer("s"))

      def processOpInto(op: Either[Unit, Boolean], into: AntiEntropy[ResettableCounter]) = op match {
        case Left(_)      => AntiEntropyContainer[ResettableCounter](into).mod(_.increment(using into.uid)())
        case Right(false) => AntiEntropyContainer[ResettableCounter](into).mod(_.decrement(using into.uid)())
        case Right(true)  => AntiEntropyContainer[ResettableCounter](into).mod(_.fresh(using into.uid)())
      }

      val ca0 = processOpInto(opA, aea)
      val cb0 = processOpInto(opB, aeb)
      processOpInto(opA, aes)
      val cs = processOpInto(opB, aes).processReceivedDeltas()

      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      assertEquals(
        ca1.data.value,
        cs.data.value,
        s"Concurrent execution of increment/decrement/fresh should have the same effect as sequential execution, but ${ca1.data.value} does not equal ${cs.data.value}"
      )
      assertEquals(
        cb1.data.value,
        cs.data.value,
        s"Concurrent execution of increment/decrement/fresh should have the same effect as sequential execution, but ${cb1.data.value} does not equal ${cs.data.value}"
      )
    }
  }

  test("concurrent reset and increment/decrement without fresh") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[ResettableCounter]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[ResettableCounter]("b", network, mutable.Buffer("a"))

    val ca0 = AntiEntropyContainer[ResettableCounter](aea).mod(_.increment(using aea.uid)())
    AntiEntropy.sync(aea, aeb)
    val cb0 = AntiEntropyContainer[ResettableCounter](aeb).processReceivedDeltas()

    val ca1 = ca0.mod(_.increment(using ca0.replicaID)())
    val cb1 = cb0.mod(_.reset())

    AntiEntropy.sync(aea, aeb)

    val ca2 = ca1.processReceivedDeltas()
    val cb2 = cb1.processReceivedDeltas()

    assertEquals(
      ca2.data.value,
      0,
      s"Concurrent reset should win over increment/decrement without fresh.\n${ca2.state}\n${cb2.state}"
    )
    assertEquals(
      cb2.data.value,
      0,
      s"Concurrent reset should win over increment/decrement without fresh.\n${cb2.state}"
    )
  }

  property("concurrent reset and increment/decrement with fresh") {
    forAll { (op: Boolean) =>
      val network = new Network(0, 0, 0)

      val aea        = new AntiEntropy[ResettableCounter]("a", network, mutable.Buffer("b"))
      val aeb        = new AntiEntropy[ResettableCounter]("b", network, mutable.Buffer("a"))
      val sequential = AntiEntropyContainer(new AntiEntropy[ResettableCounter]("c", network, mutable.Buffer("c")))

      val ca0 = AntiEntropyContainer[ResettableCounter](aea).mod(_.increment(using aea.uid)())
      AntiEntropy.sync(aea, aeb)
      val cb0 = AntiEntropyContainer[ResettableCounter](aeb).processReceivedDeltas()

      sequential.mod(_.increment(using sequential.replicaID)())

      assertEquals(ca0.data.value, sequential.data.value, s"${ca0.state} ${sequential.state}")
      assertEquals(ca0.data.value, cb0.data.value, s"${ca0.state}\n${cb0.state}")

      val ca1 =
        given rdts.syntax.LocalUid = ca0.replicaID
        if op then ca0.mod(_.fresh()).mod(_.increment()) else ca0.mod(_.fresh()).mod(_.decrement())
      val cb1 = cb0.mod(_.reset())

      AntiEntropy.sync(aea, aeb)

      val ca2 = ca1.processReceivedDeltas()
      val cb2 = cb1.processReceivedDeltas()

      sequential.mod(_.reset())
      if op then sequential.mod(_.increment(using sequential.replicaID)())
      else sequential.mod(_.decrement(using sequential.replicaID)())

      assertEquals(
        ca2.data.value,
        cb2.data.value,
        s"Concurrent increment/decrement with fresh and reset should have the same result as executing increment/decrement sequentially after reset"
      )

      assertEquals(
        ca2.data.value,
        sequential.data.value,
        s"Concurrent increment/decrement with fresh and reset should have the same result as executing increment/decrement sequentially after reset.\n${ca2.state}\n${sequential.state}"
      )
      assertEquals(
        cb2.data.value,
        sequential.data.value,
        s"Concurrent increment/decrement with fresh and reset should have the same result as executing increment/decrement sequentially after reset."
      )
    }
  }

  property("convergence") {
    forAll {
      (
          nOpsA1: (Byte, Byte, Byte, Byte),
          nOpsB1: (Byte, Byte, Byte, Byte),
          nOpsA2: (Byte, Byte, Byte, Byte),
          nOpsB2: (Byte, Byte, Byte, Byte),
          networkGen: NetworkGenerator
      ) =>
        {
          val network = networkGen.make()
          val aea     = new AntiEntropy[ResettableCounter]("a", network, mutable.Buffer("b"))
          val aeb     = new AntiEntropy[ResettableCounter]("b", network, mutable.Buffer("a"))

          val opsA1 = Random.shuffle(List.fill(nOpsA1._1.toInt)(0) ++ List.fill(nOpsA1._2.toInt)(1) ++ List.fill(
            nOpsA1._3.toInt
          )(2) ++ List.fill(nOpsA1._4.toInt)(3))
          val opsB1 = Random.shuffle(List.fill(nOpsB1._1.toInt)(0) ++ List.fill(nOpsB1._2.toInt)(1) ++ List.fill(
            nOpsB1._3.toInt
          )(2) ++ List.fill(nOpsB1._4.toInt)(3))
          val opsA2 = Random.shuffle(List.fill(nOpsA2._1.toInt)(0) ++ List.fill(nOpsA2._2.toInt)(1) ++ List.fill(
            nOpsA2._3.toInt
          )(2) ++ List.fill(nOpsA2._4.toInt)(3))
          val opsB2 = Random.shuffle(List.fill(nOpsB2._1.toInt)(0) ++ List.fill(nOpsB2._2.toInt)(1) ++ List.fill(
            nOpsB2._3.toInt
          )(2) ++ List.fill(nOpsB2._4.toInt)(3))

          def applyOps(
              counter: AntiEntropyContainer[ResettableCounter],
              ops: List[Int]
          ): AntiEntropyContainer[ResettableCounter] = {
            ops.foldLeft(counter) {
              case (c, 0) => c.mod(_.increment(using c.replicaID)())
              case (c, 1) => c.mod(_.decrement(using c.replicaID)())
              case (c, 2) => c.mod(_.reset())
              case (c, 3) => c.mod(_.fresh(using c.replicaID)())
              // default case is only needed to stop the compiler from complaining about non-exhaustive match
              case (c, _) => c
            }
          }

          val ca0 = applyOps(AntiEntropyContainer[ResettableCounter](aea), opsA1)
          val cb0 = applyOps(AntiEntropyContainer[ResettableCounter](aeb), opsB1)

          AntiEntropy.sync(aea, aeb)

          val ca1 = applyOps(ca0.processReceivedDeltas(), opsA2)
          val cb1 = applyOps(cb0.processReceivedDeltas(), opsB2)

          AntiEntropy.sync(aea, aeb)
          network.startReliablePhase()
          AntiEntropy.sync(aea, aeb)

          val ca2 = ca1.processReceivedDeltas()
          val cb2 = cb1.processReceivedDeltas()

          assert(
            ca2.data.value == cb2.data.value,
            s"After synchronization messages were reliably exchanged all replicas should converge, but ${ca2.data.value} does not equal ${cb2.data.value}"
          )
        }
    }
  }
}
