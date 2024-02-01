package loci
package communicator
package tcp

import scala.annotation.compileTimeOnly
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

@compileTimeOnly("TCP communicator only available on the JVM")
object TCP extends TCPSetupFactory {
  def unapply(tcp: TCP) = Some((tcp.host, tcp.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds,
    noDelay: Boolean = true)

  def apply(port: Int): Listener[TCP] = unavailable
  def apply(port: Int, interface: String): Listener[TCP] = unavailable
  def apply(port: Int, properties: Properties): Listener[TCP] = unavailable
  def apply(port: Int, interface: String, properties: Properties): Listener[TCP] = unavailable

  def apply(host: String, port: Int): Connector[TCP] = unavailable
  def apply(host: String, port: Int, properties: Properties): Connector[TCP] = unavailable
}

@compileTimeOnly("TCP communicator only available on the JVM")
trait TCPSetupFactory extends  ConnectionSetupFactory.Implementation[TCP] {
  val self: TCP.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties) = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: TCP.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: TCP.Properties) = unavailable
}
