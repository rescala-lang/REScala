package channel

import channel.{InChan, OutChan}
import de.rmgk.delay.Async

trait MessageBuffer {
  def asArray: Array[Byte]
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

case class Bidirectional(in: InChan, out: OutChan)
