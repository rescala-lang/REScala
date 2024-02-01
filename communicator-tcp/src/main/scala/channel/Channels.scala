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

trait InChan extends AutoCloseable {
  def receive: Async[Any, MessageBuffer]
}

trait OutChan extends AutoCloseable {
  def send(message: MessageBuffer): Async[Any, Unit]

}

case class Bidirectional(in: InChan, out: OutChan)
