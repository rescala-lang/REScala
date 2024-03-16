package channel.broadcastchannel

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{ArrayMessageBuffer, InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}
import scala.scalajs.js.annotation.*
import dom.{BroadcastChannel, MessageEvent}

class BroadcastException(message: String, event: MessageEvent) extends Exception(message)

class BroadcastChannelConnector(name: String) extends InChan with OutChan {

  val bc = new BroadcastChannel(name)

  override def receive: Prod[MessageBuffer] = Async.fromCallback {
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

  override def send(message: MessageBuffer): delay.Async[Any, Unit] =
    Sync(bc.postMessage(message.asArrayBuffer))

}
