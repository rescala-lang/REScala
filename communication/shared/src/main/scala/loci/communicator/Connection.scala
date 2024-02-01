package loci
package communicator

trait Connection[+P <: ProtocolCommon] {
  val receive: Notice.Stream[MessageBuffer]
  val closed: Notice.Steady[Unit]

  def send(message: MessageBuffer): Unit
  def close(): Unit
  def open: Boolean

  val protocol: P
}
