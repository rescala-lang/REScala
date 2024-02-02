package loci
package communicator
package ws.webnative

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}

private class WSConnector[P <: WS: WSProtocolFactory](
  url: String, properties: WS.Properties)
    extends Connector[P] {

  protected def connect(connectionEstablished: Connected[P]) = {

    val socket = new dom.WebSocket(url)

    socket.onopen = { (_: dom.Event) =>

      // protocol properties

      val (tls, host, port) = dom.document.createElement("a") match {
        case parser: dom.html.Anchor =>
          parser.href = url
          val tls = (parser.protocol compareToIgnoreCase "wss:") == 0
          val host = Some(parser.hostname)
          val port =
            try Some(parser.port.toInt)
            catch { case _: NumberFormatException => None }
          (tls, host, port)
        case _ =>
          (false, None, None)
      }

      implicitly[WSProtocolFactory[P]].make(url, host, port, this, tls, tls, tls) match {
        case Failure(exception) =>
          connectionEstablished.set(Failure(exception))

        case Success(ws) =>

          // heartbeat

          var timeoutHandle: SetTimeoutHandle = null
          var intervalHandle: SetIntervalHandle = null

          val resetTimeout = Notice.Stream[Unit]
          val resetInterval = Notice.Stream[Unit]


          // connection interface

          val doClosed = Notice.Steady[Unit]
          val doReceive = Notice.Stream[MessageBuffer]

          val connection = new Connection[P] {
            val protocol = ws
            val closed = doClosed.notice
            val receive = doReceive.notice

            def open = socket.readyState == dom.WebSocket.OPEN
            def send(data: MessageBuffer) = {
              socket.send(data.backingArrayBuffer)
              resetInterval.fire()
            }
            def close() = socket.close()
          }

          connectionEstablished.set(Success(connection))


          // heartbeat

          resetTimeout.notice foreach { _ =>
            if (timeoutHandle != null)
              clearTimeout(timeoutHandle)
            timeoutHandle = setTimeout(properties.heartbeatTimeout) {
              connection.close()
            }
          }

          resetInterval.notice foreach { _ =>
            if (intervalHandle != null)
              clearInterval(intervalHandle)
            intervalHandle = setInterval(properties.heartbeatDelay) {
              socket.send("\uD83D\uDC93")
            }
          }

          resetTimeout.fire()
          resetInterval.fire()


          // socket listeners

          socket.onmessage = { (event: dom.MessageEvent) =>
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
            resetTimeout.fire()
          }

          socket.onclose = { (event: dom.CloseEvent) =>
            clearInterval(intervalHandle)
            clearTimeout(timeoutHandle)
            doClosed.set()
          }
      }
    }

    socket.onerror = { (event: dom.Event) =>
      connectionEstablished.trySet(Failure(new ConnectionException("Websocket failed to connect")))
      socket.close()
    }
  }
}
