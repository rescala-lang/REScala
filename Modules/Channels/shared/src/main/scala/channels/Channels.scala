package channels

import de.rmgk.delay.{Async, Callback}

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

type ConnectionContext = AbstractConnectionContext[MessageBuffer]
trait AbstractConnectionContext[T] {
  def send(message: T): Async[Any, Unit]
  def close(): Unit
}

/** Provides a specification how to handle messages, given a connection context */
type AbstractIncoming[T] = AbstractConnectionContext[T] => Callback[T]
type Incoming            = AbstractIncoming[MessageBuffer]

type LatentConnection = AbstractLatentConnection[MessageBuffer]

/** Contains all the information required to try and establish a bidirectional connection.
  * Only misses specification on how to handle messages, the abstract handler is acquired from `incoming`
  * Note, may produce multiple connections (e.g., if this produces a server connection) thus triggering the async multiple times.
  *
  * Implementations should make it safe to establish multiple times, though the semantics of that is unclear.
  */
trait AbstractLatentConnection[T] {

  /** The returned async, when run, should establish connections with the given callback atomically.
    * That is, no messages should be lost.
    * Similarly, the provider of the callback (the result of `incoming`) of this method should make sure that the other end of the callback is ready to receive callbacks before running the async.
    */
  def prepare(incoming: AbstractIncoming[T]): Async[Abort, AbstractConnectionContext[T]]
}

object ConnectionMapper {

  class ConnectionMapper[A, B](f: B => A, acc: AbstractConnectionContext[A]) extends AbstractConnectionContext[B] {
    override def send(message: B): Async[Any, Unit] =
      acc.send(f(message))

    override def close(): Unit = acc.close()
  }

  def adapt[A, B](f: A => B, g: B => A)(la: AbstractLatentConnection[A]): AbstractLatentConnection[B] = {

    new AbstractLatentConnection[B] {
      def prepare(incoming: AbstractIncoming[B]): Async[Abort, AbstractConnectionContext[B]] =
        Async[Abort] {
          val conn = Async.bind:
            la.prepare: conn =>
              val mapped = ConnectionMapper(g, conn)
              val cb     = incoming(mapped)
              rs => cb.complete(rs.map(f))
          ConnectionMapper(g, conn)
        }
    }

  }
}
