package loci
package communicator
package ws.webnative

import scala.annotation.compileTimeOnly

@compileTimeOnly("Web native WebSocket communicator only available in JS")
trait WSSetupFactory extends ConnectionSetupFactory.Implementation[WS] {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties): WS.Properties = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}

@compileTimeOnly("Web native WebSocket communicator only available in JS")
trait WSSecureSetupFactory extends ConnectionSetupFactory.Implementation[WS.Secure] {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties): WS.Properties = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}
