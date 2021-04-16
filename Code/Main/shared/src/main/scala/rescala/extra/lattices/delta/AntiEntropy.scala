package rescala.extra.lattices.delta

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.collection.mutable

case class DeltaMsg[A](delta: Delta[A], seqNum: Int)
case class AckMsg(from: String, seqNum: Int)

class AntiEntropy[A: UIJDLattice]
(val replicaID: String, network: Network, neighbors: mutable.Buffer[String] = mutable.Buffer())
(implicit val codec: JsonValueCodec[A]) {

  private val deltaBufferOut: mutable.Map[Int, Delta[A]] = mutable.Map()

  private var deltaBufferIn: List[Delta[A]] = List()

  private var nextSeqNum: Int = 0

  private val ackMap: mutable.Map.WithDefault[String, Int] = new mutable.Map.WithDefault(mutable.Map(), _ => -1)

  private var fullState: A = UIJDLattice[A].bottom

  implicit val AckMsgCodec: JsonValueCodec[AckMsg] = JsonCodecMaker.make

  implicit val DeltaMsgCodec: JsonValueCodec[DeltaMsg[A]] = JsonCodecMaker.make

  type Message = Either[AckMsg, DeltaMsg[A]]

  implicit val EitherCodec: JsonValueCodec[Message] = JsonCodecMaker.make

  def addNeighbor(newNeighbor: String): Unit = {
    neighbors.append(newNeighbor)
  }

  def recordChange(delta: Delta[A], state: A): Unit = {
    fullState = state

    deltaBufferOut.update(nextSeqNum, delta)
    nextSeqNum += 1
  }

  def getReceivedDeltas: List[Delta[A]] = {
    val deltas = deltaBufferIn
    deltaBufferIn = List()
    deltas
  }

  private def receiveDelta(msg: DeltaMsg[A]): Unit = msg match {
    case DeltaMsg(delta, seqNum) =>
      deltaBufferIn = deltaBufferIn :+ delta
      val msg: Message = Left(AckMsg(replicaID, seqNum))
      network.sendMessage(delta.replicaID, writeToArray(msg))
  }

  private def receiveAck(msg: AckMsg): Unit = msg match {
    case AckMsg(from, seqNum) =>
      val maxAck = ackMap(from) max seqNum
      ackMap.update(from, maxAck)
  }

  def receiveFromNetwork(): Unit = {
    try {
      network.receiveMessages(replicaID).map(readFromArray[Message](_)).foreach {
        case Left(ackMsg) => receiveAck(ackMsg)
        case Right(deltaMsg) => receiveDelta(deltaMsg)
      }
    } catch {
      case e: JsonReaderException =>
        println("Couldn't parse message:")
        e.printStackTrace()
    }

    gc()
  }

  private def prepareDeltaMsg(to: String): Option[DeltaMsg[A]] = {
    if (deltaBufferOut.isEmpty || deltaBufferOut.keySet.min > ackMap(to))
      Some(DeltaMsg(Delta(replicaID, fullState), nextSeqNum))
    else {
      deltaBufferOut.collect {
        case (n, Delta(origin, deltaState)) if n >= ackMap(to) && origin != to => deltaState
      } reduceOption { (left: A, right: A) =>
        UIJDLattice[A].merge(left, right)
      } map { deltaState => DeltaMsg(Delta(replicaID, deltaState), nextSeqNum) }
    }
  }

  def sendChangesToAllNeighbors(): Unit = {
    neighbors.foreach { id =>
      prepareDeltaMsg(id) match {
        case None =>
        case Some(msg) =>
          val eitherMsg: Message = Right(msg)
          network.sendMessage(id, writeToArray(eitherMsg))
      }
    }
  }

  private def gc(): Unit = {
    if (ackMap.values.nonEmpty) {
      deltaBufferOut.filterInPlace {
        case (n, _) => n >= ackMap.values.min
      }
    }
  }
}

object AntiEntropy {
  def sync[A: UIJDLattice](ae: AntiEntropy[A]*): Unit = {
    ae.foreach(_.sendChangesToAllNeighbors())
    ae.foreach(_.receiveFromNetwork())
  }
}
