package channel.broadcastchannel

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{JsArrayBufferMessageBuffer, MessageBuffer, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom
import org.scalajs.dom.{BroadcastChannel, MessageEvent}

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}

class BroadcastException(message: String, event: MessageEvent) extends Exception(message)

class BroadcastChannelConnector(name: String) {

  val bc = new BroadcastChannel(name)

  def receive: Prod[MessageBuffer] = Async.fromCallback {
    bc.onmessage = (event: dom.MessageEvent) =>
      println(js.typeOf(event.data))
      event.data match
        case data: ArrayBuffer =>
          Async.handler.succeed(JsArrayBufferMessageBuffer(data))
        case other =>
          throw BroadcastException(
            s"someone put something weird on the broadcast channel ($name):\n${event.data}",
            event
          )

    bc.onmessageerror = (event: dom.MessageEvent) =>
      Async.handler.fail(BroadcastException(s"broadcast error ($name):\n${event.data}", event))
  }

  def send(message: MessageBuffer): delay.Async[Any, Unit] =
    Sync(bc.postMessage(message.asArrayBuffer))

}
