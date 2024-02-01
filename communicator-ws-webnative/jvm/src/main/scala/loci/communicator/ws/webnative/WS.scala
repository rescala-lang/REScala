package loci
package communicator
package ws.webnative

import scala.annotation.compileTimeOnly
import scala.concurrent.duration._

trait WS
    extends Protocol
    with SetupInfo
    with SecurityInfo
    with SymmetryInfo with Bidirectional {
  val url: String
  val host: Option[String]
  val port: Option[Int]

  override def toString = s"WS($url, $host, $port)"
}

@compileTimeOnly("Web native WebSocket communicator only available in JS")
object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.url, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(url: String): Connector[WS] = unavailable
  def apply(url: String, properties: Properties): Connector[WS] = unavailable

  trait Secure extends WS with communicator.Secure {
    override def toString = s"WS.Secure($url, $host, $port)"
  }

  object Secure {
    def unapply(ws: Secure) = Some((ws.url, ws.host, ws.port))

    def apply(url: String): Connector[WS.Secure] = unavailable
    def apply(url: String, properties: Properties): Connector[WS.Secure] = unavailable
  }
}
