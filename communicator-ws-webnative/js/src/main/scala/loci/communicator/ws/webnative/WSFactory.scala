package loci
package communicator
package ws.webnative

import scala.concurrent.duration.FiniteDuration

private object WSSetupParser
    extends ConnectionSetupParser
    with SimpleConnectionSetupProperties {
  val self: WS.type = WS

  def properties(implicit props: ConnectionSetupFactory.Properties) =
    WS.Properties()
      .set[FiniteDuration]("heartbeat-delay") { v => _.copy(heartbeatDelay = v) }
      .set[FiniteDuration]("heartbeat-timeout") { v => _.copy(heartbeatTimeout = v) }
}

trait WSSetupFactory extends ConnectionSetupFactory.Implementation[WS] {
  val self: WS.type = WS

  val schemes = Seq("ws", "wss")

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): WS.Properties =
    WSSetupParser.properties

  protected def listener(
      url: String, scheme: String, location: String, properties: WS.Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: WS.Properties) =
    Some(WS(url, properties))
}

trait WSSecureSetupFactory extends ConnectionSetupFactory.Implementation[WS.Secure] {
  val self: WS.type = WS

  val schemes = Seq("wss")

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): WS.Properties =
    WSSetupParser.properties

  protected def listener(
      url: String, scheme: String, location: String, properties: WS.Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: WS.Properties) =
    Some(WS.Secure(url, properties))
}
