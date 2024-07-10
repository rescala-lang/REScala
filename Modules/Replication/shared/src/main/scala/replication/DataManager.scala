package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Callback, syntax}
import rdts.base.Lattice.optionLattice
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.time.Dots
import replication.JsoniterCodecs.given
import replication.ProtocolMessage.{Payload, Request}

import java.util.Timer
import scala.util.{Failure, Success, Try}

sealed trait ProtocolMessage[+T]
object ProtocolMessage {
  case class Request(sender: Uid, knows: Dots)            extends ProtocolMessage[Nothing]
  case class Payload[T](sender: Uid, dots: Dots, data: T) extends ProtocolMessage[T]
}

case class ProtocolDots[State](data: State, context: Dots) derives Lattice, Bottom

trait Aead {
  def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte]
  def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]]
}

object DataManager {
  def jsoniterMessages[T: JsonValueCodec](conn: AbstractLatentConnection[MessageBuffer])
      : AbstractLatentConnection[ProtocolMessage[T]] = {

    given JsonValueCodec[ProtocolMessage[T]] = JsonCodecMaker.make

    ConnectionMapper.adapt(
      {
        (mb: MessageBuffer) => readFromArray[ProtocolMessage[T]](mb.asArray)
      },
      { (pm: ProtocolMessage[T]) => ArrayMessageBuffer(writeToArray(pm)) }
    )(conn)
  }
}

class DataManager[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    allChanges: ProtocolDots[State] => Unit,
    crypto: Option[Aead] = None
)(using lattice: Lattice[State], bottom: Bottom[State]) {

  given LocalUid = replicaId

  type CodecState = State

  type ConnectionContext = AbstractConnectionContext[ProtocolMessage[State]]

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
        con.send(Request(replicaId.uid, selfContext)).run(using ())(debugCallback)
    },
    10000,
    10000
  )

  def addLatentConnection(latentConnection: AbstractLatentConnection[MessageBuffer])(using
      JsonValueCodec[CodecState]
  ): Unit = {
    addLatentConnection(DataManager.jsoniterMessages(latentConnection))
  }

  def addLatentConnection(latentConnection: AbstractLatentConnection[ProtocolMessage[State]]): Unit = {
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

  private def messageBufferCallback(outChan: ConnectionContext): Callback[ProtocolMessage[State]] =
    case Success(msg)   => handleMessage(msg, outChan)
    case Failure(error) => error.printStackTrace()

  def handleMessage(msg: ProtocolMessage[State], biChan: ConnectionContext) = {
    msg match
      case Request(uid, knows) =>
        val relevant = allDeltas.filterNot { dt => dt.context <= knows }
        relevant.foreach: msg =>
          biChan.send(Payload(replicaId.uid, msg.context, msg.data)).run(using ())(debugCallback)
        updateContext(uid, selfContext merge knows)
      case Payload(uid, context, data) =>
        val interalized = ProtocolDots[State](data, context)
        lock.synchronized {
          updateContext(uid, context)
          updateContext(replicaId.uid, context)
          remoteDeltas = interalized :: remoteDeltas
        }
        receiveCallback(data)
        allChanges(interalized)

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
        con.send(Payload(replicaId.uid, delta.context, delta.data)).run(using ())(debugCallback)
  }

}
