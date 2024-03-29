package channel

import channel.{InChan, OutChan}
import de.rmgk.delay.Async

import java.nio.charset.StandardCharsets

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

class Ctx(@volatile var closeRequest: Boolean = false)

inline def context(using ctx: Ctx): Ctx = ctx

type Prod[A] = Async[Ctx, A]

trait InChan {
  def receive: Prod[MessageBuffer]
}

trait OutChan {
  def send(message: MessageBuffer): Async[Any, Unit]
}

case class BiChan(in: InChan, out: OutChan)
