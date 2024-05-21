package channel.jettywebsockets

import channel.{Abort, ArrayMessageBuffer, ConnectionContext, InChan, Incoming, LatentConnection, MessageBuffer, OutChan, Prod}
import de.rmgk.delay.{Async, syntax, Callback as DelayCallback}
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.handler.{ContextHandler, ContextHandlerCollection}
import org.eclipse.jetty.server.{Handler, Server, ServerConnector}
import org.eclipse.jetty.util.Callback as JettyUtilCallback
import org.eclipse.jetty.websocket.api.Session.Listener
import org.eclipse.jetty.websocket.api.{Frame, Session, Callback as JettyCallback}
import org.eclipse.jetty.websocket.client.WebSocketClient
import org.eclipse.jetty.websocket.server
import org.eclipse.jetty.websocket.server.{ServerUpgradeRequest, ServerUpgradeResponse, ServerWebSocketContainer, WebSocketCreator, WebSocketUpgradeHandler}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.FutureConverters.given
import java.net.URI
import java.nio.ByteBuffer
import scala.util.{Failure, Success}

def println(str: Any): Unit = System.out.println(s"$str [${Thread.currentThread().getName()}]")

object JettyWsListener {

  def prepareServer(port: Int) = {
    val server = new Server()

    val connector = new ServerConnector(server)
    connector.setPort(port)
    server.addConnector(connector)

    val jettyWsListener = fromServer(server)
    server.setHandler(jettyWsListener.handlers)
    jettyWsListener
  }

  def fromServer(server: Server) =
    new JettyWsListener(server)

}

class JettyWsListener(val server: Server) {

  val handlers = new ContextHandlerCollection()

  def listen(pathSpec: PathSpec, context: ContextHandler = new ContextHandler()): LatentConnection =
    new LatentConnection {
      override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] =
        Async.fromCallback {

          val webSocketHandler = WebSocketUpgradeHandler.from(
            server,
            context,
            (wsContainer: ServerWebSocketContainer) => {
              println(s"adding mapping")
              wsContainer.addMapping(
                pathSpec,
                webSocketCreator(incoming, Async.handler)
              )
            }
          )
          context.setHandler(webSocketHandler)
          handlers.addHandler(context)
          context.start()
        }
    }

  def webSocketCreator(incoming: Incoming, delayCallback: DelayCallback[ConnectionContext]) =
    new WebSocketCreator {
      override def createWebSocket(
          request: ServerUpgradeRequest,
          upgradeResponse: ServerUpgradeResponse,
          // callback has to be ignored if a handler is returned (great design jetty!)
          callback: JettyUtilCallback
      ): AnyRef = {
        println(s"creating ws handler")
        new JettyWsHandler(incoming, delayCallback)
      }
    }
}

object JettyWsConnection {

  def connect(uri: URI): LatentConnection = new LatentConnection {
    override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] =
      Async.fromCallback[ConnectionContext] {

        val client = new WebSocketClient()
        client.start()
        println(s"client started")
        // this returns a future
        client.connect(new JettyWsHandler(incoming, Async.handler[ConnectionContext]), uri).toAsync.run { sess =>
          println(s"connect returned $sess")
        }
      }
  }
}

class JettySessionWrapper(session: Session) extends ConnectionContext {

  override def close(): Unit =
    session.close()

  override def send(data: MessageBuffer): Async[Any, Unit] = Async.fromCallback {
    session.sendBinary(
      ByteBuffer.wrap(data.asArray),
      new JettyCallback {
        override def succeed(): Unit          = Async.handler.succeed(())
        override def fail(x: Throwable): Unit = Async.handler.fail(x)
      }
    )
  }
}

class JettyWsHandler(incoming: Incoming, connectionEstablished: DelayCallback[ConnectionContext])
    extends Listener.Abstract {

  @volatile private var internalCallback: DelayCallback[MessageBuffer] = scala.compiletime.uninitialized

  override def onWebSocketOpen(session: Session): Unit = {
    super.onWebSocketOpen(session)
    val sessionWrapper = new JettySessionWrapper(session)
    internalCallback = incoming(sessionWrapper)
    connectionEstablished.succeed(sessionWrapper)
    session.demand()
  }

  override def onWebSocketBinary(buffer: ByteBuffer, callback: JettyCallback): Unit = {

    println(s"received binary data")

    val data = new Array[Byte](buffer.remaining())
    buffer.get(data)

    internalCallback.succeed(ArrayMessageBuffer(data))

    callback.succeed()
    getSession.demand()
  }

  override def onWebSocketText(message: String): Unit = {
    println(s"received unexpected text data: $message")
    getSession.demand()
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    println(s"closing message because $reason")
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    println(s"received explicit error $cause")
    internalCallback.fail(cause)
  }

  override def onWebSocketPing(payload: ByteBuffer): Unit =
    getSession.sendPong(payload, JettyCallback.from(() => getSession.demand(), println))
}
