package rescala.extra.lattices.delta

import scala.collection.mutable
import scala.util.Random

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
