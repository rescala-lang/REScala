package tests.distribution.delta.antientropy

import kofre.decompose.containers.Network
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}

object NetworkGenerators {
  val genNetwork: Gen[Network] = for {
    lossChance      <- Gen.choose(0.0, 1.0)
    duplicateChance <- Gen.choose(0.0, 1.0)
    delayChance     <- Gen.choose(0.0, 1.0)
  } yield new Network(lossChance, duplicateChance, delayChance)

  implicit val arbNetwork: Arbitrary[Network] = Arbitrary(genNetwork)
}

class NetworkTest extends munit.ScalaCheckSuite {
  import NetworkGenerators._
  property("sendMessage/receiveMessages") {
    forAll { (msgs: List[Array[Byte]], replicaID: String) =>
      val network = new Network(0, 0, 0)
      msgs.foreach(network.sendMessage("a", _))

      val rcvdOther = network.receiveMessages(replicaID)

      assert(
        replicaID == "a" || rcvdOther.isEmpty,
        s"""For ids other than "a" no messages should be received, but $replicaID received $rcvdOther"""
      )

      val rcvd = network.receiveMessages("a")

      msgs.foreach { msg =>
        assert(
          rcvd.contains(msg),
          s"""For id "a" the sent messages should be received, but $rcvd does not contain ${msg.mkString(
              "Array(",
              ", ",
              ")"
            )}"""
        )
      }

      val rcvdAfter = network.receiveMessages("a")

      assert(
        rcvdAfter.isEmpty,
        s"After receiving the messages there should not be any messages left to received, but $rcvdAfter is not empty"
      )
    }
  }
  property("loss") {
    forAll { (msg: Array[Byte]) =>
      val network = new Network(1, 0, 0)

      network.sendMessage("a", msg)

      val rcvd = network.receiveMessages("a")

      assert(
        rcvd.isEmpty,
        s"In a network with 100% loss chance no messages should be received, but $rcvd is not empty"
      )
    }
  }
  property("duplicate") {
    forAll { (msg: Array[Byte]) =>
      val network = new Network(0, 1, 0)

      network.sendMessage("a", msg)

      val rcvd1 = network.receiveMessages("a")
      val rcvd2 = network.receiveMessages("a")
      val rcvd3 = network.receiveMessages("a")
      val rcvd4 = network.receiveMessages("a")
      val rcvd5 = network.receiveMessages("a")

      assert(
        rcvd1.contains(msg),
        s"In a network with 100% duplicate chance a message should be able to be received infinitely often, but after receiving 1 time $rcvd1 does not contain ${msg.mkString("Array(", ", ", ")")}"
      )
      assert(
        rcvd2.contains(msg),
        s"In a network with 100% duplicate chance a message should be able to be received infinitely often, but after receiving 2 time $rcvd1 does not contain ${msg.mkString("Array(", ", ", ")")}"
      )
      assert(
        rcvd3.contains(msg),
        s"In a network with 100% duplicate chance a message should be able to be received infinitely often, but after receiving 3 time $rcvd1 does not contain ${msg.mkString("Array(", ", ", ")")}"
      )
      assert(
        rcvd4.contains(msg),
        s"In a network with 100% duplicate chance a message should be able to be received infinitely often, but after receiving 4 time $rcvd1 does not contain ${msg.mkString("Array(", ", ", ")")}"
      )
      assert(
        rcvd5.contains(msg),
        s"In a network with 100% duplicate chance a message should be able to be received infinitely often, but after receiving 5 time $rcvd1 does not contain ${msg.mkString("Array(", ", ", ")")}"
      )
    }
  }
  property("delay") {
    forAll { (msg: Array[Byte]) =>
      val network = new Network(0, 0, 1)

      network.sendMessage("a", msg)
      val rcvd = network.receiveMessages("a")

      assert(
        rcvd.isEmpty,
        s"In a network with 100% delay chance no message should ever arrive, but $rcvd is not empty"
      )
    }
  }
  property("reliablePhase") {
    forAll { (msg: Array[Byte], network: Network) =>
      network.startReliablePhase()
      network.sendMessage("a", msg)

      val rcvd = network.receiveMessages("a")

      assert(
        rcvd.contains(msg),
        s"When the network is in a reliable phase all sent messages should be received, but $rcvd does not contain ${msg.mkString("Array(", ", ", ")")}"
      )
    }
  }
}
