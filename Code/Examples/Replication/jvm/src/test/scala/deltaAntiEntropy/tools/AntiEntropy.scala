package deltaAntiEntropy.tools

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tools.AntiEntropy.{AckMsg, DeltaMsg}
import kofre.base.Id.asId
import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.dotted.{Dotted, DottedDecompose}
import kofre.syntax.Named
import replication.JsoniterCodecs.given

import scala.collection.mutable

/** This class can be used together with Network to test Delta CRDTs locally. It is an implementation of the anti-entropy
  * algorithm proposed by Almeida et al. in "Delta State Replicated Data Types", see [[https://arxiv.org/pdf/1603.01529.pdf here]].
  * It also includes the modifications proposed by Enes et al. in "Efficient Synchronization of State-based CRDTs",
  * see [[https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=8731395 here]].
  *
  * To synchronize Deltas between replicas, you can either use AntiEntropy.sync or call receiveFromNetwork after sendChangesToAllNeighbors
  * on all AntiEntropy instances.
  *
  * @param replicaID Unique id of the replica that this instance is placed on
  * @param network The Network that is used for exchanging deltas with other replicas
  * @param neighbors The neighbors that this replica can communicate with directly
  * @tparam A State type of the CRDT that this anti-entropy algorithm is used with
  */
class AntiEntropy[A](
    val replicaID: String,
    network: Network,
    neighbors: mutable.Buffer[String] = mutable.Buffer()
)(implicit bottom: Bottom[A], codec: JsonValueCodec[Dotted[A]], withContextLattice: DottedDecompose[A]) {

  def state: Dotted[A] = fullState

  def uid: Id = Id.predefined(replicaID)

  private val deltaBufferOut: mutable.Map[Int, Named[Dotted[A]]] = mutable.Map()

  private var deltaBufferIn: List[Named[Dotted[A]]] = List()

  private var nextSeqNum: Int = 0

  private val ackMap: mutable.Map.WithDefault[String, Int] = new mutable.Map.WithDefault(mutable.Map(), _ => -1)

  private var fullState: Dotted[A] = Bottom.dotted.empty

  implicit val AckMsgCodec: JsonValueCodec[AckMsg] = JsonCodecMaker.make

  implicit val DeltaMsgCodec: JsonValueCodec[DeltaMsg[A]] = JsonCodecMaker.make

  type Message = Either[AckMsg, DeltaMsg[A]]

  implicit val EitherCodec: JsonValueCodec[Message] = JsonCodecMaker.make

  def addNeighbor(newNeighbor: String): Unit = {
    neighbors.append(newNeighbor)
  }

  def recordChange(delta: Named[Dotted[A]], state: Dotted[A]): Unit = {
    fullState = state

    deltaBufferOut.update(nextSeqNum, delta)
    nextSeqNum += 1
  }

  def getReceivedDeltas: List[Named[Dotted[A]]] = {
    val deltas = deltaBufferIn
    deltaBufferIn = List()
    deltas
  }

  private def receiveDelta(msg: DeltaMsg[A]): Unit = msg match {
    case DeltaMsg(delta, seqNum) =>
      deltaBufferIn = deltaBufferIn :+ delta
      val msg: Message = Left(AckMsg(replicaID, seqNum))
      network.sendMessage(kofre.base.Id.unwrap(delta.replicaId), writeToArray(msg))
  }

  private def receiveAck(msg: AckMsg): Unit = msg match {
    case AckMsg(from, seqNum) =>
      val maxAck = ackMap(from) max seqNum
      ackMap.update(from, maxAck)
  }

  def receiveFromNetwork(): Unit = {
    try {
      network.receiveMessages(replicaID).map(readFromArray[Message](_)).foreach {
        case Left(ackMsg)    => receiveAck(ackMsg)
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
      Some(DeltaMsg(Named(replicaID.asId, fullState), nextSeqNum))
    else {
      deltaBufferOut.collect {
        case (n, Named(origin, deltaState)) if n >= ackMap(to) && Id.unwrap(origin) != to => deltaState
      } reduceOption { (left: Dotted[A], right: Dotted[A]) =>
        DecomposeLattice[Dotted[A]].merge(left, right)
      } map { deltaState => DeltaMsg(Named(replicaID.asId, deltaState), nextSeqNum) }
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
  case class DeltaMsg[A](delta: Named[Dotted[A]], seqNum: Int)
  case class AckMsg(from: String, seqNum: Int)

  def sync[A](ae: AntiEntropy[A]*): Unit = {
    ae.foreach(_.sendChangesToAllNeighbors())
    ae.foreach(_.receiveFromNetwork())
  }
}
