package loci
package communicator
package ws.jetty


import org.eclipse.jetty.websocket.server.{ServerUpgradeRequest, WebSocketUpgradeHandler}

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

object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.path, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(context: WebSocketUpgradeHandler, pathspec: String): Listener[WS] =
    new WSListener[WS](context, pathspec, Properties())
  def apply(context: WebSocketUpgradeHandler, pathspec: String, properties: Properties): Listener[WS] =
    new WSListener[WS](context, pathspec, properties)

  def apply(url: String): Connector[WS] =
    new WSConnector[WS](url, Properties())
  def apply(url: String, properties: Properties): Connector[WS] =
    new WSConnector[WS](url, properties)

  trait Secure extends WS with communicator.Secure {
    override def toString = s"WS.Secure($path, $host, $port)"
  }

  object Secure {
    def unapply(ws: Secure) = Some((ws.path, ws.host, ws.port))

    def apply(url: String): Connector[WS.Secure] =
      new WSConnector[WS.Secure](url, Properties())
    def apply(url: String, properties: Properties): Connector[WS.Secure] =
      new WSConnector[WS.Secure](url, properties)
  }
}
