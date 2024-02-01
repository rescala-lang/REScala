package loci
package communicator
package ws.jetty

import loci.communicator.ConnectionSetupFactory.Implementation

import scala.annotation.compileTimeOnly

@compileTimeOnly("Jetty WebSocket communicator only available on the JVM")
trait WSSetupFactory extends ConnectionSetupFactory.Implementation[WS] {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties): WS.Properties = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}

@compileTimeOnly("Jetty WebSocket communicator only available on the JVM")
trait WSSecureSetupFactory extends Implementation[WS.Secure] {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties): WS.Properties = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}

