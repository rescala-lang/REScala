package deltaAntiEntropy.tools

import rdts.base.Uid

import scala.collection.mutable
import scala.util.Random

/** This class simulates an unreliable network that may lose, duplicate or delay messages. It was made together with [[IAntiEntropy]]
  * to locally test the correctness of Delta CRDTs in unreliable networks. The network can be made reliable for certain
  * phases only by using the startReliablePhase and endReliablePhase methods.
  *
  * @param lossChance A value between 0 and 1 describing the likelihood that a message is lost in transfer. For example,
  *                   a network with lossChance 0 will deliver every message while a network with lossChance 0.5 will lose
  *                   50% of all messages.
  * @param duplicateChance A value between 0 and 1 describing the likelihood that a received message is duplicated, i.e.
  *                        it is not removed from the internal buffer when it is passed on to the anti-entropy algorithm.
  *                        Note that a message that was left in the buffer might be duplicated again the next time that
  *                        messages are retrieved. For example, assuming a lossChance of 0, a network with duplicateChance 0
  *                        will deliver every message exactly once, while a network with duplicateChance 0.5 delivers a
  *                        message just once with a 50% chance, twice with a 25% chance, three times with a 12.5% chance
  *                        and so on.
  * @param delayChance A value between 0 and 1 describing the likelihood that a message gets delayed during transfer, i.e.
  *                    that it does not get retrieved in a call to receiveMessages. For example, assuming a lossChance of 0,
  *                    in a network with delayChance 0 every message can be retrieved in the first call to receiveMessages
  *                    after the message was sent, while in a network with delayChance 0.5 there is a 50% chance that the
  *                    message is retrieved by the first call to receiveMessages, a 25% chance for the second call, a
  *                    12.5% chance for the third call and so on.
  */
class Network(val lossChance: Double, val duplicateChance: Double, val delayChance: Double) {
  private var reliablePhase: Boolean = false

  private val reliablyTransferred: mutable.Map.WithDefault[String, List[Array[Byte]]] =
    new mutable.Map.WithDefault(mutable.Map(), _ => List())

  private val inTransfer: mutable.Map.WithDefault[String, List[Array[Byte]]] =
    new mutable.Map.WithDefault(mutable.Map(), _ => List())

  def startReliablePhase(): Unit = {
    reliablePhase = true
  }

  def endReliablePhase(): Unit = {
    reliablePhase = false
  }

  private def selectRandom(l: List[Array[Byte]], removeChance: Double): List[Array[Byte]] =
    l.zip(List.fill(l.length)(Random.between(0.0, 1.0))).collect {
      case (msg, ran) if ran > removeChance => msg
    }

  def receiveMessages(recipient: String): List[Array[Byte]] = {
    val l = inTransfer(recipient)

    val transferred = selectRandom(l, delayChance)

    val removeFromBuffer = selectRandom(transferred, duplicateChance)

    inTransfer.update(recipient, l.diff(removeFromBuffer))

    val received = reliablyTransferred.getOrElse(recipient, Nil) ++ transferred

    reliablyTransferred.remove(recipient)

    received
  }

  private def insertMessage(
      recipient: String,
      message: Array[Byte],
      into: mutable.Map[String, List[Array[Byte]]]
  ): Unit = {
    into.update(recipient, into(recipient) :+ message)
  }

  def sendMessage(recipient: String, message: Array[Byte]): Unit = {
    if (reliablePhase) {
      insertMessage(recipient, message, reliablyTransferred)
      return
    }

    if (Random.between(0.0, 1.0) <= lossChance) {
      return
    }

    insertMessage(recipient, message, inTransfer)
  }
}
