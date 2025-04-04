package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.Callback
import rdts.base.Lattice.optionLattice
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.time.Dots
import replication.JsoniterCodecs.given
import replication.ProtocolMessage.*

import scala.annotation.targetName
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

trait Aead {
  def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte]
  def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]]
}

object DeltaDissemination {
  val executeImmediately = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }
}

class DeltaDissemination[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    crypto: Option[Aead] = None,
    immediateForward: Boolean = false,
    sendingActor: ExecutionContext = DeltaDissemination.executeImmediately,
    val globalAbort: Abort = Abort(),
)(using JsonValueCodec[State]) {

  type Message = CachedMessage[ProtocolMessage[State]]

  given LocalUid = replicaId

  def pmscodec[T <: ProtocolMessage[State]]: JsonValueCodec[T] = {
    val pmscodecInv: JsonValueCodec[ProtocolMessage[State]] = JsonCodecMaker.make
    pmscodecInv.asInstanceOf[JsonValueCodec[T]]
  }

  def cachedMessages(conn: LatentConnection[MessageBuffer])
      : LatentConnection[CachedMessage[ProtocolMessage[State]]] = {
    LatentConnection.adapt(
      (mb: MessageBuffer) => ReceivedCachedMessage[ProtocolMessage[State]](mb)(using pmscodec),
      (pm: CachedMessage[ProtocolMessage[State]]) => pm.messageBuffer
    )(conn)
  }

  type ConnectionContext = Connection[Message]

  @volatile var connections: List[ConnectionContext] = Nil

  def debugCallbackAndRemoveCon(con: ConnectionContext): Callback[Any] =
    case Success(value) => ()
    case Failure(exception) =>
      lock.synchronized {
        connections = connections.filter(cc => cc != con)
      }
      println(s"exception during message handling, removing connection $con from list of connections")
      exception.printStackTrace()

  def requestData(): Unit = {
    val msg = SentCachedMessage(Request(replicaId.uid, selfContext))(using pmscodec)
    connections.foreach: con =>
      send(con, msg)
  }

  def pingAll(): Unit = {
    val msg = SentCachedMessage(Ping(System.nanoTime()))(using pmscodec)
    connections.foreach { conn =>
      send(conn, msg)
    }
  }

  @targetName("addLatentConnectionCaching")
  def addLatentConnection(latentConnection: LatentConnection[MessageBuffer]): Unit = {
    addLatentConnection(cachedMessages(latentConnection))
  }

  @targetName("addLatentConnectionPlain")
  def addLatentConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Unit = {
    addLatentConnection(LatentConnection.adapt[ProtocolMessage[State], Message](
      pm => SentCachedMessage(pm)(using pmscodec),
      cm => cm.payload
    )(latentConnection))
  }

  @targetName("addLatentConnectionCached")
  def addLatentConnection(latentConnection: LatentConnection[Message]): Unit = {
    val preparedConnection = latentConnection.prepare { from =>
      {
        case Success(msg) => handleMessage(msg, from)
        case Failure(error) =>
          println(s"exception during message handling")
          error.printStackTrace()
      }
    }
    preparedConnection.run(using globalAbort) {
      case Success(conn) =>
        lock.synchronized {
          connections = conn :: connections
        }
        send(
          conn,
          SentCachedMessage(Request(
            replicaId.uid,
            selfContext
          ))(using pmscodec)
        )
      case Failure(ex) =>
        println(s"exception during connection activation")
        ex.printStackTrace()
    }
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  val lock: AnyRef                                               = new {}
  private var pastPayloads: Queue[CachedMessage[Payload[State]]] = Queue.empty

  val keepPastPayloads = 108

  def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(pastPayloads.toList)
  private def rememberPayload(payload: CachedMessage[Payload[State]]): Unit = lock.synchronized {
    pastPayloads = pastPayloads.enqueue(payload)
    if pastPayloads.sizeIs > keepPastPayloads then
      pastPayloads = pastPayloads.drop(1)
      ()
  }

  private var contexts: Map[Uid, Dots] = Map.empty

  def selfContext: Dots = contexts.getOrElse(replicaId.uid, Dots.empty)

  def applyDelta(delta: State): Unit =
    val message = lock.synchronized {
      val nextDot = selfContext.nextDot(replicaId.uid)
      val payload = Payload(replicaId.uid, Dots.single(nextDot), delta)
      updateContext(replicaId.uid, payload.dots)
      val message = SentCachedMessage(payload)(using pmscodec)
      rememberPayload(message)
      message
    }
    disseminate(message)

  def updateContext(rr: Uid, dots: Dots): Unit = lock.synchronized {
    contexts = contexts.updatedWith(rr)(curr => curr `merge` Some(dots))
  }

  def handleMessage(msg: Message, from: ConnectionContext): Unit = {
    if globalAbort.closeRequest then return
    msg.payload match
      case Ping(time) =>
        send(from, SentCachedMessage(Pong(time))(using pmscodec))
      case Pong(time) =>
        println(s"ping took ${(System.nanoTime() - time.toLong).doubleValue / 1000_000}ms")
        println(s"current state is ${selfContext}")
      case Request(uid, knows) =>
        val relevant = allPayloads.filterNot { dt => dt.payload.dots <= knows }
        {
          val newknowledge =
            knows.merge(relevant.map { dt => dt.payload.dots }.reduceOption(Lattice.merge).getOrElse(Dots.empty))
          val diff = selfContext `subtract` newknowledge
          if !diff.isEmpty then
            throw IllegalStateException(
              s"could not answer request, missing deltas for: ${diff}\n  relevant: ${relevant}\n knows: ${knows}\n  selfcontext: ${selfContext}}"
            )
        }
        relevant.foreach: msg =>
          send(from, SentCachedMessage(msg.payload.addSender(replicaId.uid))(using pmscodec))
        updateContext(uid, selfContext `merge` knows)
      case payload @ Payload(uid, context, data) =>
        if context <= selfContext then return
        lock.synchronized {
          uid.foreach { uid =>
            updateContext(uid, context)
          }
          updateContext(replicaId.uid, context)
          rememberPayload(msg.asInstanceOf[CachedMessage[Payload[State]]])
        }
        receiveCallback(data)
        if immediateForward then
          disseminate(msg, Set(from))

  }

  def send(con: ConnectionContext, payload: Message): Unit =
    if globalAbort.closeRequest then ()
    else
      sendingActor.execute { () =>
        con.send(payload).run(using ())(debugCallbackAndRemoveCon(con))
      }

  def disseminate(payload: Message, except: Set[ConnectionContext] = Set.empty): Unit = {
    val cons = lock.synchronized(connections)
    cons.filterNot(con => except.contains(con)).foreach: con =>
      send(con, payload)

  }

}
