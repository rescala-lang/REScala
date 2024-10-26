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

import scala.util.{Failure, Success, Try}

sealed trait ProtocolMessage[+T]
object ProtocolMessage {

  /** `knows` has to be a subset of the dots known at the sender.
    * The sender of the request should then eventually receive all known missing dots.
    */
  case class Request(sender: Uid, knows: Dots) extends ProtocolMessage[Nothing]

  /** Guarantees that for two payloads a and b, that if a.dots <= b.dots,
    * then a.data <= b.data according to the lattice of T
    */
  case class Payload[T](sender: Uid, dots: Dots, data: T) extends ProtocolMessage[T]
}

case class ProtocolDots[State](data: State, context: Dots) derives Lattice, Bottom

trait Aead {
  def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte]
  def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]]
}

object DataManager {
  def jsoniterMessages[T: JsonValueCodec](conn: LatentConnection[MessageBuffer])
      : LatentConnection[ProtocolMessage[T]] = {

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
) {

  given LocalUid = replicaId

  type CodecState = State

  type ConnectionContext = Connection[ProtocolMessage[State]]

  type TransferState = ProtocolDots[State]

  val globalAbort = Abort()

  var connections: List[ConnectionContext] = Nil

  def debugCallbackAndRemoveCon(con: ConnectionContext): Callback[Any] =
    case Success(value) => ()
    case Failure(exception) =>
      lock.synchronized {
        connections = connections.filter(cc => cc != con)
      }
      exception.printStackTrace()

  def requestData(): Unit = {
    connections.foreach: con =>
      con.send(Request(replicaId.uid, selfContext)).run(using ())(debugCallbackAndRemoveCon(con))
  }

  def addLatentConnection(latentConnection: LatentConnection[MessageBuffer])(using
      JsonValueCodec[CodecState]
  ): Unit = {
    addLatentConnection(DataManager.jsoniterMessages(latentConnection))
  }

  def addLatentConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Unit = {
    println(s"activating latent connection in data manager")
    latentConnection.prepare(conn => messageBufferCallback(conn)).run(using globalAbort):
      case Success(conn) =>
        lock.synchronized {
          connections = conn :: connections
        }
      case Failure(ex) =>
        println(s"exception during connection activation")
        ex.printStackTrace()
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  val lock: AnyRef                              = new {}
  private var localDeltas: List[TransferState]  = Nil
  private var localBuffer: List[TransferState]  = Nil
  private var remoteDeltas: List[TransferState] = Nil

  private var contexts: Map[Uid, Dots] = Map.empty

  def selfContext: Dots = contexts.getOrElse(replicaId.uid, Dots.empty)

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

  def updateContext(rr: Uid, dots: Dots): Unit = lock.synchronized {
    contexts = contexts.updatedWith(rr)(curr => curr `merge` Some(dots))
  }

  private def messageBufferCallback(outChan: ConnectionContext): Callback[ProtocolMessage[State]] =
    case Success(msg)   => handleMessage(msg, outChan)
    case Failure(error) => error.printStackTrace()

  def handleMessage(msg: ProtocolMessage[State], biChan: ConnectionContext): Unit = {
    msg match
      case Request(uid, knows) =>
        val relevant = allDeltas.filterNot { dt => dt.context <= knows }
        relevant.foreach: msg =>
          biChan.send(Payload(replicaId.uid, msg.context, msg.data)).run(using ())(debugCallbackAndRemoveCon(biChan))
        updateContext(uid, selfContext `merge` knows)
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

  def disseminateLocalBuffer(): Unit = {
    val deltas = lock.synchronized {
      val deltas = localBuffer
      localBuffer = Nil
      localDeltas = deltas ::: localDeltas
      deltas
    }
    connections.foreach: con =>
      deltas.foreach: delta =>
        con.send(Payload(replicaId.uid, delta.context, delta.data)).run(using ())(debugCallbackAndRemoveCon(con))
  }

}
