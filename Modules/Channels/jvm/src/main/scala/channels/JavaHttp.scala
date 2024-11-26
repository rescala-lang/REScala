package channels

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import de.rmgk.delay.{Async, Callback, Sync, syntax}
import rdts.base.{LocalUid, Uid}

import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.given

object JavaHttp {

  class IncorrectSetupException extends Exception

  class SSEServerConnection(out: JioOutputStreamAdapter) extends Connection[MessageBuffer] {
    override def send(message: MessageBuffer): Async[Any, Unit] = Async { out.send(message) }
    override def close(): Unit                                  = out.outputStream.close()
  }

  val replicaIdHeader = "x-replica-id"

  class SSEServer(addHandler: HttpHandler => Unit) extends LatentConnection[MessageBuffer] {

    var connections: Map[Uid, Callback[MessageBuffer]] = Map.empty

    def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async.fromCallback {

      addHandler { (exchange: HttpExchange) =>
        val requestHeaders = exchange.getRequestHeaders
        exchange.getResponseHeaders.add("Access-Control-Allow-Origin", "*")
        exchange.getResponseHeaders.add("Access-Control-Allow-Methods", "POST, GET")
        exchange.getResponseHeaders.add("Access-Control-Allow-Headers", "x-replica-id")
        val uid = Option(requestHeaders.get(replicaIdHeader)).flatMap(_.asScala.headOption) match
          case None =>
            println(s"no replica ID on request?")
            Uid.zero
          case Some(rid) =>
            Uid.predefined(rid)

        if Option(requestHeaders.get("Accept")).flatMap(_.asScala.headOption).contains("text/event-stream")
        then
          val responseHeaders = exchange.getResponseHeaders
          responseHeaders.add("Content-Type", "text/event-stream")
          responseHeaders.add("Connection", "keep-alive")

          exchange.sendResponseHeaders(200, 0)
          val outstream = exchange.getResponseBody

          // force sending of response headers
          outstream.flush()

          val conn = SSEServerConnection(JioOutputStreamAdapter(outstream))

          SSEServer.this.synchronized {
            println(s"made connection for $uid")
            connections = connections.updated(uid, receiver.messageHandler(conn))
          }

          Async.handler.succeed(conn)
        else if exchange.getRequestMethod == "POST"
        then
          SSEServer.this.synchronized {
            connections.get(uid)
          } match
            case None =>
              println(s"received message without connection …")
              Async.handler.fail(IncorrectSetupException())
            case Some(cb) =>
              cb.succeed(ArrayMessageBuffer(exchange.getRequestBody.readAllBytes()))
              exchange.sendResponseHeaders(200, 0)
              exchange.close()
        else
          exchange.sendResponseHeaders(200, 0)
          exchange.close()
      }
    }
  }

  class SSEClientConnection(client: HttpClient, uri: URI, localUid: LocalUid) extends Connection[MessageBuffer] {

    override def send(message: MessageBuffer): Async[Any, Unit] = Async {
      val sseRequest = HttpRequest.newBuilder()
        .POST(BodyPublishers.ofByteArray(message.asArray))
        .setHeader(replicaIdHeader, localUid.uid.delegate)
        .uri(uri)
        .build()

      val res = client.sendAsync(sseRequest, BodyHandlers.discarding()).toAsync.bind

    }
    override def close(): Unit = ()
  }

  class SSEClient(client: HttpClient, uri: URI, replicaId: LocalUid, ec: ExecutionContext)
      extends LatentConnection[MessageBuffer] {

    def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async {

      val sseRequest = HttpRequest.newBuilder()
        .GET()
        .uri(uri)
        .setHeader(replicaIdHeader, replicaId.uid.delegate)
        .setHeader("Accept", "text/event-stream")
        .build()

      println(s"sending client request")

      val res = client.sendAsync(sseRequest, BodyHandlers.ofInputStream()).toAsync.bind
      val rec = res.body()

      println(s"acquired body")

      val conn = SSEClientConnection(client, uri, replicaId)

      ec.execute(() => JioInputStreamAdapter(rec).loopReceive(receiver.messageHandler(conn)))

      println(s"succeeding client")

      conn

    }
  }

}
