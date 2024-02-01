package loci
package communicator
package tcp

import scala.concurrent.duration._

trait TCP
    extends Protocol
    with SetupInfo
    with SecurityInfo
    with SymmetryInfo with Bidirectional {
  val host: String
  val port: Int

  override def toString = s"TCP($host, $port)"
}

object TCP extends TCPSetupFactory {
  def unapply(tcp: TCP) = Some((tcp.host, tcp.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds,
    noDelay: Boolean = true)

  def apply(port: Int): Listener[TCP] =
    new TCPListener(port, "localhost", Properties())
  def apply(port: Int, interface: String): Listener[TCP] =
    new TCPListener(port, interface, Properties())
  def apply(port: Int, properties: Properties): Listener[TCP] =
    new TCPListener(port, "localhost", properties)
  def apply(port: Int, interface: String, properties: Properties): Listener[TCP] =
    new TCPListener(port, interface, properties)

  def apply(host: String, port: Int): Connector[TCP] =
    new TCPConnector(host, port, Properties())
  def apply(host: String, port: Int, properties: Properties): Connector[TCP] =
    new TCPConnector(host, port, properties)
}
