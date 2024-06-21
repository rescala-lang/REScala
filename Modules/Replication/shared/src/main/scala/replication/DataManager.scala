package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Callback, syntax}
import rdts.base.Lattice.optionLattice
import rdts.base.{Bottom, Lattice, Uid}
import rdts.syntax.LocalUid
import rdts.time.Dots
import replication.JsoniterCodecs.given
import replication.ProtocolMessage.{Payload, Request}

import java.util.Timer
import scala.util.{Failure, Success}

class Binding[T](key: String)(using lat: Lattice[T], codec: JsonValueCodec[T])

case class HMap(keys: Map[String, Binding[?]], values: Map[String, Any])

sealed trait ProtocolMessage[+T]
object ProtocolMessage {
  case class Request(sender: Uid, knows: Dots) extends ProtocolMessage[Nothing]
  case class Payload[T](sender: Uid, data: T)  extends ProtocolMessage[T]
}

case class ProtocolDots[State](data: State, context: Dots) derives Lattice, Bottom

trait Crypto {
  def encrypt(data: Array[Byte]): Array[Byte]
  def decrypt(data: Array[Byte]): Array[Byte]
}

class DataManager[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    allChanges: ProtocolDots[State] => Unit,
    crypto: Option[Crypto] = None
)(using jsonCodec: JsonValueCodec[State], lattice: Lattice[State], bottom: Bottom[State]) {

  given protocolDotsCodec: JsonValueCodec[ProtocolDots[State]]              = JsonCodecMaker.make
  given protocolCodec: JsonValueCodec[ProtocolMessage[ProtocolDots[State]]] = JsonCodecMaker.make

  given LocalUid = replicaId

  type TransferState = ProtocolDots[State]

  val globalAbort = Abort()

  var connections: List[ConnectionContext] = Nil

  val timer = new Timer()

  val debugCallback: Callback[Any] =
    case Success(value)     => ()
    case Failure(exception) => exception.printStackTrace()

  timer.scheduleAtFixedRate(
    { () =>
      connections.foreach: con =>
        con.send(missingRequestMessage).run(using ())(debugCallback)
    },
    10000,
    10000
  )

  private def messageBufferCallback(outChan: ConnectionContext): Callback[MessageBuffer] =
    case Success(msg) =>
      val bytes = crypto.map(c => c.decrypt(msg.asArray)).getOrElse(msg.asArray)
      val res   = readFromArray[ProtocolMessage[TransferState]](bytes)
      println(s"$res")
      handleMessage(res, outChan)
    case Failure(error) => error.printStackTrace()

  def addLatentConnection(latentConnection: LatentConnection): Unit = {
    println(s"activating latent connection in data manager")
    latentConnection.prepare(conn => messageBufferCallback(conn)).run(using globalAbort):
      case Success(conn) =>
        lock.synchronized {
          connections = conn :: connections
        }
      case Failure(ex) => ex.printStackTrace()
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  val lock: AnyRef                              = new {}
  private var localDeltas: List[TransferState]  = Nil
  private var localBuffer: List[TransferState]  = Nil
  private var remoteDeltas: List[TransferState] = Nil

  private var contexts: Map[Uid, Dots] = Map.empty

  def selfContext = contexts.getOrElse(replicaId.uid, Dots.empty)

  def applyLocalDelta(dotted: ProtocolDots[State]): Unit = lock.synchronized {
    localBuffer = dotted :: localBuffer
    updateContext(replicaId.uid, dotted.context)
    allChanges(dotted)
    disseminateLocalBuffer()
  }

  def applyUnrelatedDelta(delta: State): Unit = lock.synchronized {
    val nextDot = selfContext.nextDot(replicaId.uid)
    applyLocalDelta(ProtocolDots(delta, Dots.single(nextDot)))
  }

  def allDeltas: List[ProtocolDots[State]] = lock.synchronized {
    List(localBuffer, remoteDeltas, localDeltas).flatten
  }

  def updateContext(rr: Uid, dots: Dots) = lock.synchronized {
    contexts = contexts.updatedWith(rr)(curr => curr merge Some(dots))
  }

  def handleMessage(msg: ProtocolMessage[TransferState], biChan: ConnectionContext) = {
    msg match
      case Request(uid, knows) =>
        val relevant = allDeltas.filterNot { dt => dt.context <= knows }
        relevant.map(encodeDelta).foreach: msg =>
          biChan.send(msg).run(using ())(debugCallback)
        updateContext(uid, selfContext merge knows)
      case Payload(uid, named) =>
        lock.synchronized {
          updateContext(uid, named.context)
          updateContext(replicaId.uid, named.context)
          remoteDeltas = named :: remoteDeltas
        }
        receiveCallback(named.data)
        allChanges(named)

  }

  def disseminateLocalBuffer() = {
    val deltas = lock.synchronized {
      val deltas = localBuffer
      localBuffer = Nil
      localDeltas = deltas ::: localDeltas
      deltas
    }
    connections.foreach: con =>
      deltas.foreach: delta =>
        con.send(encodeDelta(delta)).run(using ())(debugCallback)
  }

  def encodeDelta(delta: TransferState): MessageBuffer = {
    toEncBuffer:
      writeToArray[ProtocolMessage[TransferState]](Payload(replicaId.uid, delta))
  }

  def missingRequestMessage: MessageBuffer = {
    toEncBuffer:
      writeToArray[ProtocolMessage[TransferState]](Request(replicaId.uid, selfContext))
  }

  def toEncBuffer(bytes: Array[Byte]): MessageBuffer =
    val enc = crypto.map(c => c.encrypt(bytes)).getOrElse(bytes)
    ArrayMessageBuffer(enc)

}
