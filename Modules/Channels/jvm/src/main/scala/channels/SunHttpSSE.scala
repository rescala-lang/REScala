package channels

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import de.rmgk.delay.Async

import scala.concurrent.ExecutionContext

object SunHttpSSE {

  class SSEServer(addHandler: HttpHandler => Unit, ec: ExecutionContext) extends LatentConnection[MessageBuffer] {
    def prepare(incomingHandler: Handler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async.fromCallback {

      addHandler { (exchange: HttpExchange) =>
        val headers = exchange.getResponseHeaders
        headers.add("Content-Type", "text/event-stream")
        headers.add("Connection", "keep-alive")

        exchange.sendResponseHeaders(200, 0)
        val outstream = exchange.getResponseBody

        val instream = exchange.getRequestBody

        val conn = JIOStreamConnection(instream, outstream, () => exchange.close())

        ec.execute(() => conn.loopHandler(incomingHandler))

        Async.handler.succeed(conn)
      }
    }
  }

}
