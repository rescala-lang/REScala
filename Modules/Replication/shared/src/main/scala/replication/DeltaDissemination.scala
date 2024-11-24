package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Callback, syntax}
import rdts.base.Lattice.optionLattice
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.time.Dots
import replication.JsoniterCodecs.given
import replication.ProtocolMessage.*

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
  case class Payload[+T](senders: Set[Uid], dots: Dots, data: T) extends ProtocolMessage[T] {
    def addSender(s: Uid) = copy(senders = senders + s)
  }
  object Payload {
    def apply[T](sender: Uid, dots: Dots, data: T): Payload[T] = Payload(Set(sender), dots, data)

    // this kinda makes sense, but kinda does not
    // given [T: Lattice]: Lattice[Payload[T]] = Lattice.derived
  }

  case class Ping(time: Long) extends ProtocolMessage[Nothing]
  case class Pong(time: Long) extends ProtocolMessage[Nothing]
}

case class ProtocolDots[State](data: State, context: Dots) derives Lattice, Bottom

trait Aead {
  def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte]
  def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]]
}

object DeltaDissemination {
  def jsoniterMessages[T: JsonValueCodec](conn: LatentConnection[MessageBuffer])
      : LatentConnection[ProtocolMessage[T]] = {

    given JsonValueCodec[ProtocolMessage[T]] = JsonCodecMaker.make

    LatentConnection.adapt(
      (mb: MessageBuffer) => readFromArray[ProtocolMessage[T]](mb.asArray),
      (pm: ProtocolMessage[T]) => ArrayMessageBuffer(writeToArray(pm))
    )(conn)
  }
}

class DeltaDissemination[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    crypto: Option[Aead] = None,
    immediateForward: Boolean = false,
) {

  given LocalUid = replicaId

  type ConnectionContext = Connection[ProtocolMessage[State]]

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

  def pingAll(): Unit = {
    connections.foreach { conn =>
      conn.send(Ping(System.nanoTime())).run(debugCallbackAndRemoveCon(conn))
    }
  }

  def addLatentConnection(latentConnection: LatentConnection[MessageBuffer])(using
      JsonValueCodec[State]
  ): Unit = {
    addLatentConnection(DeltaDissemination.jsoniterMessages(latentConnection))
  }

  def addLatentConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Unit = {
    latentConnection.prepare(conn => messageBufferCallback(conn)).run(using globalAbort) {
      case Success(conn) =>
        lock.synchronized {
          connections = conn :: connections
        }
        conn.send(Request(replicaId.uid, selfContext)).run(using ())(debugCallbackAndRemoveCon(conn))
      case Failure(ex) =>
        println(s"exception during connection activation")
        ex.printStackTrace()
    }
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  val lock: AnyRef                               = new {}
  private var pastPayloads: List[Payload[State]] = Nil

  def allPayloads: List[Payload[State]] = pastPayloads

  private var contexts: Map[Uid, Dots] = Map.empty

  def selfContext: Dots = contexts.getOrElse(replicaId.uid, Dots.empty)

  def applyDelta(delta: State): Unit =
    val payload = lock.synchronized {
      val nextDot = selfContext.nextDot(replicaId.uid)
      val payload = Payload(replicaId.uid, Dots.single(nextDot), delta)
      updateContext(replicaId.uid, payload.dots)
      pastPayloads = payload :: pastPayloads
      payload
    }
    disseminate(payload)

  def updateContext(rr: Uid, dots: Dots): Unit = lock.synchronized {
    contexts = contexts.updatedWith(rr)(curr => curr `merge` Some(dots))
  }

  private def messageBufferCallback(outChan: ConnectionContext): Callback[ProtocolMessage[State]] =
    case Success(msg)   => handleMessage(msg, outChan)
    case Failure(error) => error.printStackTrace()

  def handleMessage(msg: ProtocolMessage[State], biChan: ConnectionContext): Unit = {
    msg match
      case Ping(time) =>
        biChan.send(Pong(time)).run(debugCallbackAndRemoveCon(biChan))
      case Pong(time) =>
        println(s"ping took ${(System.nanoTime() - time.toLong).doubleValue / 1000_000}ms")
      case Request(uid, knows) =>
        val relevant = pastPayloads.filterNot { dt => dt.dots <= knows }
        relevant.foreach: msg =>
          biChan.send(msg.addSender(replicaId.uid)).run(using ())(debugCallbackAndRemoveCon(biChan))
        updateContext(uid, selfContext `merge` knows)
      case payload @ Payload(uid, context, data) =>
        if context <= selfContext then return
        lock.synchronized {
          uid.foreach { uid =>
            updateContext(uid, context)
          }
          updateContext(replicaId.uid, context)
          pastPayloads = payload :: pastPayloads
        }
        receiveCallback(data)
        if immediateForward then
          disseminate(payload, Set(biChan))

  }

  def disseminate(payload: Payload[State], except: Set[ConnectionContext] = Set.empty): Unit = {
    connections.filterNot(con => except.contains(con)).foreach: con =>
      con.send(payload).run(using ())(debugCallbackAndRemoveCon(con))
  }

}
