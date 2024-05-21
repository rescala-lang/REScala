package channel

import de.rmgk.delay.{Async, Callback}

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}

trait MessageBuffer {
  def asArray: Array[Byte]
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

trait InChan {
  def receive: Prod[MessageBuffer]
}

trait OutChan {
  def send(message: MessageBuffer): Async[Any, Unit]
}

trait ConnectionContext {
  def send(message: MessageBuffer): Async[Any, Unit]
  def close(): Unit
}

type Incoming = ConnectionContext => Callback[MessageBuffer]

/** Contains all the information required to try and establish a bidirectional connection.
  * Only misses specification on how to handle messages.
  * Note, may produce multiple connections (e.g., if this produces a server connection) thus triggering the async multiple times.
  *
  * Implementations should make it safe to establish multiple times, though the semantics of that is unclear.
  */
trait LatentConnection {
  def prepare(incoming: Incoming): Async[Abort, ConnectionContext]
}

case class BiChan(in: InChan, out: OutChan)
