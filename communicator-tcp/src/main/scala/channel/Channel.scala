package channel

import channel.{InChan, OutChan}
import de.rmgk.delay.Async

trait MessageBuffer {
  def asArray: Array[Byte]
  def length: Int
}

case class ArrayMessageBuffer(inner: Array[Byte]) extends MessageBuffer {
  override def asArray: Array[Byte] = inner
  override def length: Int          = inner.length
}

trait Channel {
  def close(): Async[Any, Unit]
  def closed: Async[Any, Boolean]
}

trait InChan extends Channel {
  def receive: Async[Any, MessageBuffer]
}

trait OutChan extends Channel {
  def send(message: MessageBuffer): Async[Any, Unit]

}

case class Bidirectional(in: InChan, out: OutChan)

