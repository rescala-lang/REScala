package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.client.WebSocketClient

import java.net.URI
import scala.util.{Failure, Success}

private class WSConnector[P <: WS : WSProtocolFactory](
  url: String, properties: WS.Properties)
    extends Connector[P] {

  protected def connect(connectionEstablished: Connected[P]): Unit = {
    val uri = URI.create(url)
    val tls = uri.getScheme == "wss"

    implicitly[WSProtocolFactory[P]].make(
        url, Some(uri.getHost), Some(uri.getPort),
        this, tls, tls, tls, None) match {
      case Failure(exception) =>
        connectionEstablished.set(Failure(exception))

      case Success(ws) =>
        val socket = new Socket(ws, properties)(connectionEstablished.trySet(_), connectionEstablished.trySet(_))

        val client = new WebSocketClient()
        client.start()
        client.connect(socket, uri)
        socket.doClosed.notice foreach { _ =>
          client.stop()
        }
    }
  }
}

