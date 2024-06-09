package replication

import channels.{Abort, ArrayMessageBuffer, ConnectionContext, LatentConnection, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Callback, syntax}
import rdts.base.Lattice.optionLattice
import rdts.base.{Bottom, Lattice, Uid}
import rdts.dotted.{Dotted, DottedLattice, HasDots}
import rdts.syntax.{LocalUid, PermCausalMutate}
import rdts.time.Dots
import reactives.default.{Event, Evt, Signal, Var}
import replication.JsoniterCodecs.given
import replication.ProtocolMessage.{Payload, Request}

import java.nio.charset.StandardCharsets
import java.util.Timer
import scala.annotation.unused
import scala.collection.{View, mutable}
import scala.util.{Failure, Success}

class Key[T](@unused name: String)(using @unused lat: DottedLattice[T], @unused hado: HasDots[T])

case class HMap(keys: Map[String, Key[?]], values: Map[String, Any])

sealed trait ProtocolMessage[+T]
object ProtocolMessage {
  case class Request(sender: Uid, knows: Dots) extends ProtocolMessage[Nothing]
  case class Payload[T](sender: Uid, data: T)  extends ProtocolMessage[T]
}

case class ProtocolDots[State](data: State, context: Dots) derives Lattice, Bottom

class DataManager[State](
    val replicaId: LocalUid,
)(using jsonCodec: JsonValueCodec[State], lattice: Lattice[State], bottom: Bottom[State]) {

  given protocolCodec: JsonValueCodec[ProtocolMessage[ProtocolDots[State]]] = JsonCodecMaker.make
  given protocolDotsCodec: JsonValueCodec[ProtocolDots[State]]              = JsonCodecMaker.make

  given LocalUid = replicaId

  type TransferState = ProtocolDots[State]

  var connections: List[ConnectionContext] = Nil

  val timer = new Timer()

  val tick: Var[Int] = Var(0)

  val debugCallback: Callback[Any] =
    case Success(value)     => ()
    case Failure(exception) => exception.printStackTrace()

  timer.scheduleAtFixedRate(
    { () =>
      tick.transform(_ + 1)
      connections.foreach: con =>
        con.send(missingRequestMessage).run(using ())(debugCallback)
    },
    4000,
    4000
  )

  private def messageBufferCallback(outChan: ConnectionContext): Callback[MessageBuffer] =
    case Success(msg) =>
      val res = readFromArray[ProtocolMessage[TransferState]](msg.asArray)
      println(s"$res")
      handleMessage(res, outChan)
    case Failure(error) => error.printStackTrace()

  def addLatentConnection(latentConnection: LatentConnection): Unit = {
    println(s"activating latent connection in data manager")
    latentConnection.prepare(conn => messageBufferCallback(conn)).run(using Abort()):
      case Success(conn) =>
        lock.synchronized {
          connections = conn :: connections
        }
      case Failure(ex) => ex.printStackTrace()
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  private val lock: AnyRef                      = new {}
  private var localDeltas: List[TransferState]  = Nil
  private var localBuffer: List[TransferState]  = Nil
  private var remoteDeltas: List[TransferState] = Nil

  private val contexts: Var[Map[Uid, Dots]]                            = Var(Map.empty)
  private val filteredContexts: mutable.Map[Uid, Signal[Option[Dots]]] = mutable.Map.empty

  def peerids: Signal[Set[Uid]] = contexts.map(_.keySet)
  def contextOf(rr: Uid): Signal[Dots] = Signal {
    contexts.value.getOrElse(rr, Dots.empty)
  }

  private val changeEvt             = Evt[TransferState]()
  val changes: Event[TransferState] = changeEvt
  val mergedState                   = changes.fold(Bottom.empty[ProtocolDots[State]]) { (curr, ts) => curr merge ts }
  val currentContext: Signal[Dots]  = mergedState.map(_.context)

  val encodedStateSize = mergedState.map(s => writeToArray(s).size)

  def applyLocalDelta(dotted: ProtocolDots[State]): Unit = lock.synchronized {
    localBuffer = dotted :: localBuffer
    changeEvt.fire(dotted)
    disseminateLocalBuffer()
  }

  def transform(fun: State => State) = lock.synchronized {
    val current = mergedState.now
    applyLocalDelta(ProtocolDots(fun(current.data), Dots.single(current.context.nextDot(replicaId.uid))))
  }

  def allDeltas: List[ProtocolDots[State]] = lock.synchronized {
    List(localBuffer, remoteDeltas, localDeltas).flatten
  }

  def updateRemoteContext(rr: Uid, dots: Dots) = {
    contexts.transform(_.updatedWith(rr)(curr => curr merge Some(dots)))
  }

  def handleMessage(msg: ProtocolMessage[TransferState], biChan: ConnectionContext) = {
    msg match
      case Request(uid, knows) =>
        val relevant = allDeltas.filterNot(dt => dt.context <= knows)
        relevant.map(encodeDelta).foreach: msg =>
          biChan.send(msg).run(using ())(debugCallback)
        updateRemoteContext(uid, currentContext.now merge knows)
      case Payload(uid, named) =>
        lock.synchronized {
          updateRemoteContext(uid, named.context)
          remoteDeltas = named :: remoteDeltas
        }
        changeEvt.fire(named)

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
        con.send(encodeDelta(delta))
  }

  def encodeDelta(delta: TransferState): MessageBuffer =
    ArrayMessageBuffer(writeToArray[ProtocolMessage[TransferState]](Payload(replicaId.uid, delta)))

  def disseminateFull() =
    connections.foreach: con =>
      con.send(encodeDelta(mergedState.now))

  def encodeNamed[A: JsonValueCodec](name: String, value: A) =
    ArrayMessageBuffer(s"$name\r\n\r\n".getBytes(StandardCharsets.UTF_8) ++ writeToArray(value))

  def missingRequestMessage: MessageBuffer =
    ArrayMessageBuffer(writeToArray[ProtocolMessage[TransferState]](Request(replicaId.uid, currentContext.now)))

}
