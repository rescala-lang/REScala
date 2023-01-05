package deltaAntiEntropy.tests

import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyCRDT}
import kofre.decompose.containers.Network
import kofre.decompose.interfaces.RCounterInterface.{RCounter, RCounterSyntax}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.*

import scala.collection.mutable
import scala.util.Random

object RCounterGenerators {
  def genRCounter: Gen[AntiEntropyCRDT[RCounter]] = for {
    num <- Gen.choose(0, 100)
    ops <- Gen.listOfN(num, Gen.chooseNum(0, 3))
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[RCounter]("a", network, mutable.Buffer())

    ops.foldLeft(AntiEntropyCRDT[RCounter](ae)) {
      case (c, 0) => c.increment()
      case (c, 1) => c.decrement()
      case (c, 2) => c.reset()
      case (c, 3) => c.fresh()
      // default case is only needed to stop the compiler from complaining about non-exhaustive match
      case (c, _) => c
    }
  }

  implicit def arbRCounter: Arbitrary[AntiEntropyCRDT[RCounter]] = Arbitrary(genRCounter)
}

class RCounterTest extends munit.ScalaCheckSuite {
  import RCounterGenerators.*

  property("increment") {
    forAll { (counter: AntiEntropyCRDT[RCounter]) =>
      val orig        = counter.value
      val incremented = counter.increment()

      assert(
        incremented.value == orig + 1,
        s"Incrementing the counter should increase its value by 1, but ${incremented.value} does not equal ${counter.value} + 1"
      )
    }
  }

  property("decrement") {
    forAll { (counter: AntiEntropyCRDT[RCounter]) =>
      val orig        = counter.value
      val decremented = counter.decrement()

      assert(
        decremented.value == orig - 1,
        s"Decrementing the counter should decrease its value by 1, but ${decremented.value} does not equal ${counter.value} - 1"
      )
    }
  }

  property("fresh") {
    forAll { (counter: AntiEntropyCRDT[RCounter]) =>
      val orig  = counter.value
      val fresh = counter.fresh()

      assert(
        fresh.value == orig,
        s"Calling fresh should not change the value of the counter, but ${fresh.value} does not equal ${counter.value}"
      )
    }
  }

  property("reset") {
    forAll { (counter: AntiEntropyCRDT[RCounter]) =>
      val reset = counter.reset()

      assertEquals(reset.value, 0, s"${reset.state}")
    }
  }

  property("concurrent increment/decrement/fresh") {
    forAll { (opA: Either[Unit, Boolean], opB: Either[Unit, Boolean]) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[RCounter]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[RCounter]("b", network, mutable.Buffer("a"))
      val aes = new AntiEntropy[RCounter]("s", network, mutable.Buffer("s"))

      def processOpInto(op: Either[Unit, Boolean], into: AntiEntropy[RCounter]) = op match {
        case Left(_)      => AntiEntropyCRDT[RCounter](into).increment()
        case Right(false) => AntiEntropyCRDT[RCounter](into).decrement()
        case Right(true)  => AntiEntropyCRDT[RCounter](into).fresh()
      }

      val ca0 = processOpInto(opA, aea)
      val cb0 = processOpInto(opB, aeb)
      processOpInto(opA, aes)
      val cs = processOpInto(opB, aes).processReceivedDeltas()

      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      assertEquals(
        ca1.value,
        cs.value,
        s"Concurrent execution of increment/decrement/fresh should have the same effect as sequential execution, but ${ca1.value} does not equal ${cs.value}"
      )
      assertEquals(
        cb1.value,
        cs.value,
        s"Concurrent execution of increment/decrement/fresh should have the same effect as sequential execution, but ${cb1.value} does not equal ${cs.value}"
      )
    }
  }

  test("concurrent reset and increment/decrement without fresh") {
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[RCounter]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[RCounter]("b", network, mutable.Buffer("a"))

    val ca0 = AntiEntropyCRDT[RCounter](aea).increment()
    AntiEntropy.sync(aea, aeb)
    val cb0 = AntiEntropyCRDT[RCounter](aeb).processReceivedDeltas()

    val ca1 = ca0.increment()
    val cb1 = cb0.reset()

    AntiEntropy.sync(aea, aeb)

    val ca2 = ca1.processReceivedDeltas()
    val cb2 = cb1.processReceivedDeltas()

    assertEquals(
      ca2.value,
      0,
      s"Concurrent reset should win over increment/decrement without fresh.\n${ca2.state}\n${cb2.state}"
    )
    assertEquals(
      cb2.value,
      0,
      s"Concurrent reset should win over increment/decrement without fresh.\n${cb2.state}"
    )
  }

  property("concurrent reset and increment/decrement with fresh") {
    forAll { (op: Boolean) =>
      val network = new Network(0, 0, 0)

      val aea        = new AntiEntropy[RCounter]("a", network, mutable.Buffer("b"))
      val aeb        = new AntiEntropy[RCounter]("b", network, mutable.Buffer("a"))
      val sequential = AntiEntropyCRDT(new AntiEntropy[RCounter]("c", network, mutable.Buffer("c")))

      val ca0 = AntiEntropyCRDT[RCounter](aea).increment()
      AntiEntropy.sync(aea, aeb)
      val cb0 = AntiEntropyCRDT[RCounter](aeb).processReceivedDeltas()

      sequential.increment()

      assertEquals(ca0.value, sequential.value, s"${ca0.state} ${sequential.state}")
      assertEquals(ca0.value, cb0.value, s"${ca0.state}\n${cb0.state}")

      val ca1 = if (op) ca0.fresh().increment() else ca0.fresh().decrement()
      val cb1 = cb0.reset()

      AntiEntropy.sync(aea, aeb)

      val ca2 = ca1.processReceivedDeltas()
      val cb2 = cb1.processReceivedDeltas()

      sequential.reset()
      if (op) sequential.increment() else sequential.decrement()

      assertEquals(
        ca2.value,
        cb2.value,
        s"Concurrent increment/decrement with fresh and reset should have the same result as executing increment/decrement sequentially after reset"
      )

      assertEquals(
        ca2.value,
        sequential.value,
        s"Concurrent increment/decrement with fresh and reset should have the same result as executing increment/decrement sequentially after reset.\n${ca2.state}\n${sequential.state}"
      )
      assertEquals(
        cb2.value,
        sequential.value,
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
          network: Network
      ) =>
        {
          val aea = new AntiEntropy[RCounter]("a", network, mutable.Buffer("b"))
          val aeb = new AntiEntropy[RCounter]("b", network, mutable.Buffer("a"))

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

          def applyOps(counter: AntiEntropyCRDT[RCounter], ops: List[Int]): AntiEntropyCRDT[RCounter] = {
            ops.foldLeft(counter) {
              case (c, 0) => c.increment()
              case (c, 1) => c.decrement()
              case (c, 2) => c.reset()
              case (c, 3) => c.fresh()
              // default case is only needed to stop the compiler from complaining about non-exhaustive match
              case (c, _) => c
            }
          }

          val ca0 = applyOps(AntiEntropyCRDT[RCounter](aea), opsA1)
          val cb0 = applyOps(AntiEntropyCRDT[RCounter](aeb), opsB1)

          AntiEntropy.sync(aea, aeb)

          val ca1 = applyOps(ca0.processReceivedDeltas(), opsA2)
          val cb1 = applyOps(cb0.processReceivedDeltas(), opsB2)

          AntiEntropy.sync(aea, aeb)
          network.startReliablePhase()
          AntiEntropy.sync(aea, aeb)

          val ca2 = ca1.processReceivedDeltas()
          val cb2 = cb1.processReceivedDeltas()

          assert(
            ca2.value == cb2.value,
            s"After synchronization messages were reliably exchanged all replicas should converge, but ${ca2.value} does not equal ${cb2.value}"
          )
        }
    }
  }
}
