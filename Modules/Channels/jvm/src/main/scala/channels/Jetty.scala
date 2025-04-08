package channels.jettywebsockets

import channels.{Abort, ArrayMessageBuffer, Connection, LatentConnection, MessageBuffer, Receive as ChannelHandler}
import de.rmgk.delay.{Async, toAsync, Callback as DelayCallback}
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.handler.{ContextHandler, ContextHandlerCollection}
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.util.Callback as JettyUtilCallback
import org.eclipse.jetty.websocket.api.Session.Listener
import org.eclipse.jetty.websocket.api.{Session, Callback as JettyCallback}
import org.eclipse.jetty.websocket.client.WebSocketClient
import org.eclipse.jetty.websocket.server
import org.eclipse.jetty.websocket.server.*

import java.net.URI
import java.nio.ByteBuffer
import scala.util.{Failure, Success}

def println(str: Any): Unit = System.out.println(s"$str [${Thread.currentThread().getName()}]")

object JettyWsListener {

  def prepareServer(port: Int): JettyWsListener = {
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

  def listen(pathSpec: PathSpec, context: ContextHandler = new ContextHandler()): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incomingHandler: ChannelHandler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
        Async.fromCallback {

          val webSocketHandler = WebSocketUpgradeHandler.from(
            server,
            context,
            (wsContainer: ServerWebSocketContainer) => {
              println(s"adding mapping")
              wsContainer.addMapping(
                pathSpec,
                webSocketCreator(incomingHandler, Async.handler)
              )
            }
          )
          context.setHandler(webSocketHandler)
          handlers.addHandler(context)
          context.start()
        }
    }

  def webSocketCreator(
      incoming: ChannelHandler[MessageBuffer],
      delayCallback: DelayCallback[Connection[MessageBuffer]]
  ) =
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

  def connect(uri: URI): LatentConnection[MessageBuffer] = new LatentConnection {
    override def prepare(incomingHandler: ChannelHandler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
      Async.fromCallback[Connection[MessageBuffer]] {

        val client = new WebSocketClient()
        client.start()
        println(s"client started")
        // this returns a future
        client.connect(new JettyWsHandler(incomingHandler, Async.handler[Connection[MessageBuffer]]), uri).toAsync.run(using ()) {
          sess =>
            println(s"connect returned $sess")
        }
      }
  }
}

class JettySessionWrapper(session: Session) extends Connection[MessageBuffer] {

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

class JettyWsHandler(
    incoming: ChannelHandler[MessageBuffer],
    connectionEstablished: DelayCallback[Connection[MessageBuffer]]
) extends Listener.Abstract {

  @volatile private var internalCallback: DelayCallback[MessageBuffer] = scala.compiletime.uninitialized

  override def onWebSocketOpen(session: Session): Unit = {
    super.onWebSocketOpen(session)
    val sessionWrapper = new JettySessionWrapper(session)
    internalCallback = incoming.messageHandler(sessionWrapper)
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
