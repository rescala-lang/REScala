package loci
package communicator
package broadcastchannel

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}
import scala.scalajs.js.annotation._

@js.native
@JSGlobal("BroadcastChannel")
class JSBroadcastChannel(name: String) extends js.Object {
  def postMessage(value: js.Any): Unit = js.native

  def addEventListener(`type`: String, callback: js.Function1[dom.MessageEvent, _]): Unit = js.native

  def close(): Unit = js.native
}

private class BroadcastChannelConnector[P <: BroadcastChannel: BroadcastChannelProtocolFactory](
  name: String, properties: BroadcastChannel.Properties)
    extends Connector[P] {

  protected def connect(connectionEstablished: Connected[P]) = {
    val bc = new JSBroadcastChannel(name)
    
    implicitly[BroadcastChannelProtocolFactory[P]].make(name, this) match {
      case Failure(exception) =>
        connectionEstablished.set(Failure(exception))

      case Success(p) =>

        val doClosed = Notice.Steady[Unit]
        val doReceive = Notice.Stream[MessageBuffer]

        val connection = new Connection[P] {
          val protocol = p
          val closed = doClosed.notice
          val receive = doReceive.notice

          def open = true
          def send(data: MessageBuffer) = {
            bc.postMessage(data.backingArrayBuffer)
          }
          def close() = {
            bc.close()
            doClosed.set()
          }
        }

        connectionEstablished.set(Success(connection))

        bc.addEventListener("message", { (event: dom.MessageEvent) =>
          event.data match {
            case data: ArrayBuffer =>
              doReceive.fire(MessageBuffer wrapArrayBuffer data)

            case data: dom.Blob =>
              val reader = new dom.FileReader
              reader.onload = { (event: dom.Event) =>
                doReceive.fire(MessageBuffer wrapArrayBuffer
                  event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer])
              }
              reader.readAsArrayBuffer(data)

            case _ =>
          }
        })

        bc.addEventListener("messageerror", { (event: dom.MessageEvent) =>
          connectionEstablished.trySet(Failure(new ConnectionException("BroadcastChannel failed to receive message")))
          bc.close()
          doClosed.set()
        })
    }
  }
}
