package todo

import channels.{Abort, ArrayMessageBuffer, Connection, LatentConnection, MessageBuffer}
import de.rmgk.delay.{Async, Sync}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object WebviewAdapterChannel {

  var receiveCallback: String => String = identity

  @JSExportTopLevel("webview_channel_receive")
  def receive(msg: String) = receiveCallback(msg)

  object WebviewConnectionContext extends Connection[MessageBuffer] {
    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val b64 = new String(java.util.Base64.getEncoder.encode(message.asArray))
      if !js.isUndefined(scala.scalajs.js.Dynamic.global.webview_channel_send) then
        println(s"sending message to webview")
        scala.scalajs.js.Dynamic.global.webview_channel_send(b64)
        ()
      else
        println(s"webview channel send was undefined :(")
      // w.eval(s"""webview_channel_receive('$b64')""")
    }
    override def close(): Unit = ()
  }

  def listen(): LatentConnection[MessageBuffer] = new LatentConnection {
    def prepare(incomingHandler: Handler): Async[Abort, Connection[MessageBuffer]] = Sync {
      val conn = WebviewConnectionContext
      val cb   = incomingHandler(conn)
      receiveCallback = { (msg: String) =>
        val bytes = java.util.Base64.getDecoder.decode(msg)
        cb.succeed(ArrayMessageBuffer(bytes))
        ""
      }

      conn
    }
  }
}
