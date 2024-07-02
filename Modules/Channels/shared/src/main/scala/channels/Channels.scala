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

inline def context(using ctx: Abort): Abort = ctx

type Prod[A] = Async[Abort, A]

trait ConnectionContext {
  def send(message: MessageBuffer): Async[Any, Unit]
  def close(): Unit
}

/** Provides a specification how to handle messages, given a connection context */
type Incoming = ConnectionContext => Callback[MessageBuffer]

/** Contains all the information required to try and establish a bidirectional connection.
  * Only misses specification on how to handle messages, the abstract handler is acquired from `incoming`
  * Note, may produce multiple connections (e.g., if this produces a server connection) thus triggering the async multiple times.
  *
  * Implementations should make it safe to establish multiple times, though the semantics of that is unclear.
  */
trait LatentConnection {

  /** The returned async, when run, should establish connections with the given callback atomically.
    * That is, no messages should be lost.
    * Similarly, the provider of the callback (the result of `incoming`) of this method should make sure that the other end of the callback is ready to receive callbacks before running the async.
    */
  def prepare(incoming: Incoming): Async[Abort, ConnectionContext]
}
