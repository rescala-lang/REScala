package channels

import de.rmgk.delay.{Async, Callback}
import rdts.base.Uid

import java.nio.charset.StandardCharsets

trait MessageBuffer {
  def asArray: Array[Byte]
  def show: String = asArray.mkString("[", ", ", "]")
}

object MessageBuffer {
  given Conversion[String, MessageBuffer] = str => ArrayMessageBuffer(str.getBytes(StandardCharsets.UTF_8))
  given Conversion[MessageBuffer, String] = buf => new String(buf.asArray, StandardCharsets.UTF_8)

}

case class ArrayMessageBuffer(inner: Array[Byte]) extends MessageBuffer {
  override def asArray: Array[Byte] = inner
}

class Abort(@volatile var closeRequest: Boolean = false)

type Prod[A] = Async[Abort, A]

case class ConnectionInfo(hostname: Option[String], port: Option[Int])

/** Connections are bidirectional. Receiving is handled by the incoming handler of the latent connection. */
trait Connection[T] {
  // TODO: currently not consistently implemented
  def info: ConnectionInfo                    = ConnectionInfo(None, None)
  def authenticatedPeerReplicaId: Option[Uid] = None
  def send(message: T): Async[Any, Unit]
  def close(): Unit
}

/** Provides a specification how to handle messages, given a connection context.
  * Failure calls on the callback generally indicate connection errors on the receiver side.
  */
trait Receive[T] {

  /** The provided connection is not guaranteed to be useable until the first message is received.
    * If you want to initiate sending messages on this connection, use the value returned by the prepare call of the latent connection instead.
    */
  def messageHandler(answers: Connection[T]): Callback[T]
}

/** Contains all the information required to try and establish a bidirectional connection.
  * Only misses specification on how to handle messages, the abstract handler is acquired from `incoming`
  * Note, may produce multiple connections (e.g., if this produces a server connection) thus triggering the async multiple times.
  *
  * Implementations should make it safe to establish multiple times, though the semantics of that is unclear.
  */
trait LatentConnection[T] {

  /** The returned async, when run, should establish connections with the given callback atomically.
    * That is, no messages should be lost during setup.
    * Similarly, the provider of the callback (the result of `incoming`) of this method should make sure that the other end of the callback is ready to receive callbacks before running the async.
    *
    * It is generally not assumed to be safe to run prepare twice (neither running a single async twice, nor running two different returned asyncs).
    * Notably, “server” like implementations may try to bind a specific port, and immediately fail if that is not available.
    *
    * The async may produce multiple connections and will run [[incomingHandler]] for each of them.
    */
  def prepare(receiver: Receive[T]): Async[Abort, Connection[T]]
}

object LatentConnection {

  class EncodingConnection[A, B](f: B => A, acc: Connection[A]) extends Connection[B] {
    override def send(message: B): Async[Any, Unit] =
      acc.send(f(message))

    override def close(): Unit = acc.close()
  }

  def adapt[A, B](f: A => B, g: B => A)(latentConnection: LatentConnection[A]): LatentConnection[B] = {
    new LatentConnection[B] {
      def prepare(receiver: Receive[B]): Async[Abort, Connection[B]] =
        Async[Abort] {
          val conn = Async.bind {
            latentConnection.prepare { conn =>
              val mapped = EncodingConnection(g, conn)
              val cb     = receiver.messageHandler(mapped)
              rs => cb.complete(rs.map(f))
            }
          }
          EncodingConnection(g, conn)
        }
    }
  }
}
