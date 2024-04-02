package replication

import channel.{ArrayMessageBuffer, BiChan, Ctx, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromSubArray, writeToArray}
import rdts.syntax.LocalReplicaId
import rdts.base.Uid
import replication.CMessage.{Identify, Payload}
import replication.JsoniterCodecs.given
import de.rmgk.delay.Async
import reactives.operator.{Evt, Fold, Signal}
import reactives.default.{act, current}
import de.rmgk.delay.syntax.run

import java.nio.charset.StandardCharsets
import java.util.concurrent.{ArrayBlockingQueue, BlockingDeque}
import scala.util.chaining.scalaUtilChainingOps

class PeerRef[T](uid: Uid) {
  val messageQueue: ArrayBlockingQueue[T] = new ArrayBlockingQueue[T](Int.MaxValue)
}

sealed trait CMessage[T]
object CMessage {
  case class Identify(uid: Uid)                          extends CMessage[Nothing]
  case class Payload[T](contentType: String, payload: T) extends CMessage[T]
}

class MessageCoder[T](codecs: Map[String, JsonValueCodec[T]]) {

  def encodePath[A: JsonValueCodec](name: String, value: A) =
    ArrayMessageBuffer(s"$name\r\n\r\n".getBytes(StandardCharsets.UTF_8) ++ writeToArray(value))

  def decode(message: MessageBuffer): Identify | Payload[T] = {
    val messageArray = message.asArray
    val pos          = messageArray.indexOfSlice("\r\n\r\n".getBytes(StandardCharsets.UTF_8))
    if pos <= 0 then throw IllegalStateException(s"Received invalid message")
    else
      val name = new String(messageArray.slice(0, pos), StandardCharsets.UTF_8)
      println(s"handling received message for $name")
      name match
        case "identify" => Identify(readFromSubArray[Uid](messageArray, pos + 4, messageArray.length))
        case other =>
          val codec = codecs(other)
          Payload("other", readFromSubArray(messageArray, pos + 4, messageArray.length)(using codec))
  }

  def encode(msg: CMessage[T]): MessageBuffer =
    msg match
      case Identify(uid)  => encodePath("identify", uid)
      case Payload(ct, t) => encodePath(ct, t)(using codecs(ct))
}

class ConnectionManager(lrid: LocalReplicaId) {

  val addPeerRef: Evt[PeerRef[?]] = Evt()

  val peers: Signal[List[PeerRef[?]]] = Fold(List.empty){
    addPeerRef act {_ :: current}
  }

  def addConnection[T](connection: BiChan, coder: MessageCoder[T]): Async[Ctx, Unit] = {
    Async[Ctx] {
      var peerRef: PeerRef[T] = null
      val msg                 = connection.in.receive.bind
      coder.decode(msg) match
        case Identify(uid) =>
          peerRef = PeerRef(uid)
          addPeerRef.fire(peerRef)
        case Payload(ct, t) =>
          peerRef.messageQueue.put(t)
    }
  }

}
