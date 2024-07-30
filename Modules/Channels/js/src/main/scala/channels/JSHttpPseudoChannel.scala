package channels

import channels.MesageBufferExtensions.asArrayBuffer
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback}
import org.scalajs.dom.{EventSource, Headers, HttpMethod, MessageEvent, ReadableStream, ReadableStreamReader, RequestInit, fetch}
import rdts.base.LocalUid

import java.net.URI
import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.util.chaining.scalaUtilChainingOps

object JSHttpPseudoChannel {

  class SSEPseudoConnection(uri: String, rid: LocalUid) extends Connection[MessageBuffer] {
    override def send(message: MessageBuffer): Async[Any, Unit] = Async {

      val requestInit = new RequestInit {}.tap: ri =>
        ri.method = HttpMethod.POST
        ri.body = message.asArrayBuffer
        ri.headers = Headers().tap: hi =>
          hi.set("x-replica-id", rid.uid.delegate)

      val res = fetch(uri, requestInit).toFuture.toAsync.bind
    }
    override def close(): Unit = ()
  }

  class StreamConsumer(reader: ReadableStreamReader[Uint8Array], cb: Callback[MessageBuffer]) {
    var buffer: Array[Byte] = Array.empty

    def loop(): Async[Any, Unit] = Async {
      val chunk = reader.read().toFuture.toAsync.bind
      val input = chunk.value
      buffer = buffer.appendedAll(new Int8Array(input.buffer, input.byteOffset, input.length).toArray)

      if buffer.length >= 4 then
        val len = ByteBuffer.wrap(buffer.slice(0, 4)).getInt()
        if buffer.length >= len + 4 then
          val mb = ArrayMessageBuffer(buffer.slice(4, len + 4))
          buffer = buffer.slice(len + 4, buffer.length)
          cb.succeed(mb)

      loop().bind

    }

  }

  def connect(uri: String, rid: LocalUid): LatentConnection[MessageBuffer] = new LatentConnection[MessageBuffer] {
    def prepare(incomingHandler: Handler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async {

      val conn = new SSEPseudoConnection(uri, rid)
      val cb   = incomingHandler.getCallbackFor(conn)

      val requestInit = new RequestInit {}.tap: ri =>
        ri.method = HttpMethod.GET
        ri.headers = Headers().tap: hi =>
          hi.set("x-replica-id", rid.uid.delegate)
          hi.set("Accept", "text/event-stream")

      val res = fetch(uri, requestInit).toFuture.toAsync.bind

      val reader = res.body.getReader()

      StreamConsumer(reader, cb).loop().run(using ())(_ => ())

      conn

    }

  }

}
