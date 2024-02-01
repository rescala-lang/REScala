package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.server.{ServerUpgradeRequest, WebSocketUpgradeHandler}

import scala.annotation.compileTimeOnly
import scala.concurrent.duration._

trait WS
    extends Protocol
    with SetupInfo
    with SecurityInfo
    with SymmetryInfo with Bidirectional {
  val path: String
  val host: Option[String]
  val port: Option[Int]
  val request: Option[ServerUpgradeRequest]

  override def toString = s"WS($path, $host, $port)"
}

@compileTimeOnly("Jetty WebSocket communicator only available on the JVM")
object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.path, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(context: WebSocketUpgradeHandler, pathspec: String): Listener[WS] = unavailable
  def apply(context: WebSocketUpgradeHandler, pathspec: String, properties: Properties): Listener[WS] = unavailable

  def apply(url: String): Connector[WS] = unavailable
  def apply(url: String, properties: Properties): Connector[WS] = unavailable

  trait Secure extends WS with communicator.Secure {
    override def toString = s"WS.Secure($path, $host, $port)"
  }

  object Secure {
    def unapply(ws: Secure) = Some((ws.path, ws.host, ws.port))

    def apply(url: String): Connector[WS.Secure] = unavailable
    def apply(url: String, properties: Properties): Connector[WS.Secure] = unavailable
  }
}
