package channel.webnative

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom
import org.scalajs.dom.Blob

import java.io.IOException
import java.nio.ByteBuffer
import scala.scalajs.js
import scala.scalajs.js.timers.*
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}
import scala.scalajs.js.typedarray.TypedArrayBufferOps.*
import scala.scalajs.js.typedarray.given
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, TypedArrayBuffer, Uint8Array}

class WebsocketException(msg: String) extends IOException(msg)

class WebsocketConnect(socket: dom.WebSocket) extends InChan with OutChan {

  def open(): Boolean = socket.readyState == dom.WebSocket.OPEN

  def send(data: MessageBuffer): Async[Any, Unit] = Sync {
    socket.send(data.asArrayBuffer)
  }

  def close() = socket.close()

  override def receive: Prod[MessageBuffer] = Async.fromCallback {

    socket.onmessage = { (event: dom.MessageEvent) =>
      event.data match {
        case data: ArrayBuffer =>
          Async.handler.succeed(JsArrayBufferMessageBuffer(data))

        case data: dom.Blob =>
          val reader = new dom.FileReader
          reader.onload = { (event: dom.Event) =>
            val buffer = event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer]
            Async.handler.succeed(JsArrayBufferMessageBuffer(buffer))
          }
          reader.readAsArrayBuffer(data)

        case _ =>
      }
    }

    socket.onerror = (event: dom.Event) =>
      socket.close()
      Async.handler.fail(new WebsocketException("Websocket failed to connect"))
  }

}

object WebsocketConnect {

  def connect(url: String): Async[Any, WebsocketConnect] = Async.fromCallback {

    val socket = new dom.WebSocket(url)

    socket.onopen = (_: dom.Event) =>
      Async.handler.succeed:
        new WebsocketConnect(socket)

    socket.onerror = (event: dom.Event) =>
      socket.close()
      Async.handler.fail(new WebsocketException("Websocket failed to connect"))
  }
}
