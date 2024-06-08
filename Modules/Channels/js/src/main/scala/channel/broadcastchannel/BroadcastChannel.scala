package channel.broadcastchannel

import channel.*
import channel.MesageBufferExtensions.asArrayBuffer
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom
import org.scalajs.dom.{BroadcastChannel, MessageEvent}

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}

class BroadcastException(message: String, event: MessageEvent) extends Exception(message)

object BroadcastChannelConnector {
  def named(name: String): LatentConnection = new LatentConnection {
    override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Async {

      val bc         = new BroadcastChannel(name)
      val connection = BroadcastChannelConnection(bc)

      val handler = incoming(connection)

      bc.onmessage = (event: dom.MessageEvent) =>
        println(js.typeOf(event.data))
        event.data match
          case data: ArrayBuffer =>
            handler.succeed(JsArrayBufferMessageBuffer(data))
          case other =>
            handler.fail(
              BroadcastException(
                s"someone put something weird on the broadcast channel ($name):\n${event.data}",
                event
              )
            )

      bc.onmessageerror = (event: dom.MessageEvent) =>
        handler.fail(BroadcastException(s"broadcast error ($name):\n${event.data}", event))

      connection
    }

  }
}

class BroadcastChannelConnection(bc: BroadcastChannel) extends ConnectionContext {

  def send(message: MessageBuffer): delay.Async[Any, Unit] =
    Sync(bc.postMessage(message.asArrayBuffer))

  override def close(): Unit = ()

}
