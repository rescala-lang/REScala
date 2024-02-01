package loci
package communicator
package ws.jetty

import org.eclipse.jetty.util.Callback
import org.eclipse.jetty.websocket.server
import org.eclipse.jetty.websocket.server.{
  ServerUpgradeResponse, ServerWebSocketContainer, WebSocketCreator, WebSocketUpgradeHandler
}

import java.util.function.Consumer
import scala.util.{Failure, Success, Try}

private class WSListener[P <: WS: WSProtocolFactory](
    webSocketUpgradeHandler: WebSocketUpgradeHandler,
    pathspec: String,
    properties: WS.Properties
) extends Listener[P] {
  self =>

  protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
    webSocketUpgradeHandler.configure {
      new Consumer[ServerWebSocketContainer] {
        override def accept(wsContainer: ServerWebSocketContainer): Unit = {
          wsContainer.addMapping(
            pathspec,
            new WebSocketCreator {
              override def createWebSocket(
                  request: server.ServerUpgradeRequest,
                  upgradeResponse: ServerUpgradeResponse,
                  callback: Callback
              ): AnyRef = {
                val uri = request.getHttpURI
                val tls = uri.getScheme == "wss"

                implicitly[WSProtocolFactory[P]].make(
                  request.getHttpURI.toString,
                  Some(uri.getHost),
                  Some(uri.getPort),
                  self,
                  tls,
                  tls,
                  tls,
                  Some(request)
                ) match {
                  case Failure(exception) =>
                    connectionEstablished.fire(Failure(exception))
                    null

                  case Success(ws) =>
                    new Socket[P](ws, properties)(connectionEstablished.fire, Function.const(()))
                }
              }
            }
          )
        }
      }
    }

    Success(new Listening {
      def stopListening() = ()
    })
  }
}
