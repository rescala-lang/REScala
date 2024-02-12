package loci
package registry

import communicator.ws.jetty._
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.websocket.server.WebSocketUpgradeHandler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Jetty12WebSocketRegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Jetty WebSocket Registry"

  val port = 45851

  def run(test: RegistryTests.Test) = {

    val server     = new Server()

    val connector = new ServerConnector(server)
    connector.setPort(port)
    server.addConnector(connector)

    val context = new ContextHandler()
    server.setHandler(context)

    val webSocketHandler = WebSocketUpgradeHandler.from(server, context)
    context.setHandler(webSocketHandler)

    test(
      WS(webSocketHandler, "/registry/*"),
      WS(s"ws://localhost:$port/registry/"),
      { server.start(); true },
      true,
      server.stop()
    )
  }

  it should "handle binding and lookup correctly" in {
    for (_ <- 1 to 50)
      run(RegistryTests.`handle binding and lookup correctly`)
  }

  it should "handle subjective binding and lookup correctly" in {
    for (_ <- 1 to 50)
      run(RegistryTests.`handle subjective binding and lookup correctly`)
  }
}
