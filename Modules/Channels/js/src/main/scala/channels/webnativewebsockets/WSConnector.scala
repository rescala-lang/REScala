package channels.webnativewebsockets

import channels.*
import channels.MesageBufferExtensions.asArrayBuffer
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom

import java.io.IOException
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}

class WebsocketException(msg: String) extends IOException(msg)

class WebsocketConnect(socket: dom.WebSocket) extends Connection[MessageBuffer] {

  def open(): Boolean = socket.readyState == dom.WebSocket.OPEN

  def send(data: MessageBuffer): Async[Any, Unit] = Sync {
    socket.send(data.asArrayBuffer)
  }

  def close() = socket.close()

}

object WebsocketConnect {

  def connect(url: String): LatentConnection[MessageBuffer] = new LatentConnection {

    override def prepare(incomingHandler: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
      Async.fromCallback {

        println(s"preparing connection")

        val socket = new dom.WebSocket(url)

        socket.onopen = (_: dom.Event) => {

          println(s"connection opened")

          val connect  = new WebsocketConnect(socket)
          val callback = incomingHandler.messageHandler(connect)

          socket.onmessage = { (event: dom.MessageEvent) =>
            event.data match {
              case data: ArrayBuffer =>
                callback.succeed(JsArrayBufferMessageBuffer(data))

              case data: dom.Blob =>
                val reader = new dom.FileReader
                reader.onload = { (event: dom.Event) =>
                  val buffer = event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer]
                  callback.succeed(JsArrayBufferMessageBuffer(buffer))
                }
                reader.readAsArrayBuffer(data)

              case data: String =>
                callback.succeed(ArrayMessageBuffer(data.getBytes))
            }
          }

          socket.onerror = (event: dom.Event) =>
            socket.close()
            callback.fail(new WebsocketException("Error during websocket communication"))

          Async.handler.succeed(connect)
        }

        socket.onerror = (event: dom.Event) =>
          socket.close()
          Async.handler.fail(new WebsocketException("Websocket failed to connect"))
      }
  }
}
