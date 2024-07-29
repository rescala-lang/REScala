package channels

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import de.rmgk.delay.Async

import java.io.{PipedInputStream, PipedOutputStream}
import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import scala.concurrent.ExecutionContext
import scala.util.Using

object SunHttpSSE {

  class SSEServer(addHandler: HttpHandler => Unit, ec: ExecutionContext) extends LatentConnection[MessageBuffer] {
    def prepare(incomingHandler: Handler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async.fromCallback {

      addHandler { (exchange: HttpExchange) =>
        val headers = exchange.getResponseHeaders
        headers.add("Content-Type", "text/event-stream")
        headers.add("Connection", "keep-alive")

        exchange.sendResponseHeaders(200, 0)
        val outstream = exchange.getResponseBody


        val conn = JIOStreamConnection(null, outstream, () => exchange.close())

        Async.handler.succeed(conn)
      }
    }
  }

}

object SSEConnection {

  class SSEClient(client: HttpClient, uri: URI, ec: ExecutionContext) extends LatentConnection[MessageBuffer] {

    def prepare(incomingHandler: Handler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async.fromCallback {

      val request = HttpRequest.newBuilder()
        .GET()
        .uri(uri)
        .setHeader("Accept", "text/event-stream")
        .build()

      println(s"sending client request")

      val res = client.send(request, BodyHandlers.ofInputStream())
      val rec = res.body()

      println(s"acquired body")

      val conn = JIOStreamConnection(rec, null, () => rec.close())

      ec.execute(() => conn.loopHandler(incomingHandler))

      println(s"succeeding client")

      Async.handler.succeed(conn)

      ()

    }
  }

}
