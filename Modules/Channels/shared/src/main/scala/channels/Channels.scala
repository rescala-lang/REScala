package channels

import channels.LatentConnection.EncodingConnection
import de.rmgk.delay.{Async, Callback}

import java.nio.charset.StandardCharsets

trait MessageBuffer {
  def asArray: Array[Byte]
  def show: String = asArray.mkString("[", ", ", "]")
}

object MessageBuffer {
  given Conversion[String, MessageBuffer] = str => ArrayMessageBuffer(str.getBytes(StandardCharsets.UTF_8))
  given Conversion[MessageBuffer, String] = buf => new String(buf.asArray, StandardCharsets.UTF_8)

  type Handler = Connection[MessageBuffer] => Callback[MessageBuffer]
}

case class ArrayMessageBuffer(inner: Array[Byte]) extends MessageBuffer {
  override def asArray: Array[Byte] = inner
}

class Abort(@volatile var closeRequest: Boolean = false)

type Prod[A] = Async[Abort, A]

case class ConnectionInfo(hostname: Option[String], port: Option[Int])

/** Connections are bidirectional. Receiving is handled by the `AbstractLatentConnection.incomingHandler`. */
trait Connection[T] {
  // TODO: currently not consistently implemented
  def info: ConnectionInfo = ConnectionInfo(None, None)
  def send(message: T): Async[Any, Unit]
  def close(): Unit
}

/** Provides a specification how to handle messages, given a connection context */
trait Handler[T] {
  def getCallbackFor(conn: Connection[T]): Callback[T]
}

/** Contains all the information required to try and establish a bidirectional connection.
  * Only misses specification on how to handle messages, the abstract handler is acquired from `incoming`
  * Note, may produce multiple connections (e.g., if this produces a server connection) thus triggering the async multiple times.
  *
  * Implementations should make it safe to establish multiple times, though the semantics of that is unclear.
  */
trait LatentConnection[T] {

  /** The returned async, when run, should establish connections with the given callback atomically.
    * That is, no messages should be lost.
    * Similarly, the provider of the callback (the result of `incoming`) of this method should make sure that the other end of the callback is ready to receive callbacks before running the async.
    */
  def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]]
}

object LatentConnection {

  class EncodingConnection[A, B](f: B => A, acc: Connection[A]) extends Connection[B] {
    override def send(message: B): Async[Any, Unit] =
      acc.send(f(message))

    override def close(): Unit = acc.close()
  }

  def adapt[A, B](f: A => B, g: B => A)(latentConnection: LatentConnection[A]): LatentConnection[B] = {
    new LatentConnection[B] {
      def prepare(incomingHandler: Handler[B]): Async[Abort, Connection[B]] =
        Async[Abort] {
          val conn = Async.bind {
            latentConnection.prepare { conn =>
              val mapped = EncodingConnection(g, conn)
              val cb     = incomingHandler.getCallbackFor(mapped)
              rs => cb.complete(rs.map(f))
            }
          }
          EncodingConnection(g, conn)
        }
    }
  }
}
