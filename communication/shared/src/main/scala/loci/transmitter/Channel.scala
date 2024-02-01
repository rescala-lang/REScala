package loci
package transmitter

trait Channel {
  val receive: Notice.Stream[MessageBuffer]
  val closed: Notice.Steady[Unit]

  def send(message: MessageBuffer): Unit
  def close(): Unit
  def open: Boolean
}
