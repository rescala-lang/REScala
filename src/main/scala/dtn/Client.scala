package dtn

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.capabilities
import sttp.model.Uri
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.ws.WebSocket
import sttp.client3.pekkohttp.PekkoHttpBackend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.concurrent.ConcurrentLinkedQueue


// we need this for Promise/Future in scalajs
import scala.scalajs.js.Thenable.Implicits._


object Client {
  private val ip: String = "127.0.0.1"
  private val port: String = "3000"
  private val http_api: String = s"http://$ip:$port"
  private val ws_url: String = s"ws://$ip:$port/ws"

  private val backend: SttpBackend[Future, WebSockets] = PekkoHttpBackend()

  // get request wrapper to send an URI to the DTN deamon synchronously and read the response
  private def uget(uri: Uri): String = Await.result(backend.send(basicRequest.get(uri).response(asStringAlways)), Duration.Inf).body

  val nodeId: String = uget(uri"$http_api/status/nodeid")
  
  /*
  We need this compat class because scala.js does not support java.lang.Thread and will not do so in the future as js is single-threaded.
  As we need to make sure only one concurrent call on a WebSocket is active and we are single-threaded anyways,
  the simplest and most efficient solution is make the interface synchronous.
  */
  class WS_JsCompat {
    private var registeredServices: List[String] = List()

    private val ws: WebSocket[Future] = Await.result(backend.send(basicRequest.get(uri"$ws_url").response(asWebSocketAlwaysUnsafe)), Duration.Inf).body

    private def command(text: String): String = {
      val result = Promise[String]
      val result_future = result.future
      ws.sendText(text).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get)))
      Await.result(result_future, Duration.Inf)
    }

    def receiveBundle(): Bundle = {
      val result = Promise[Bundle]
      val result_future = result.future
      ws.receiveBinary(true).onComplete(response => result.success((Bundle.createFromDtnWsJson(response.get))))
      Await.result(result_future, Duration.Inf)
    }

    def sendBundle(bundle: Bundle): String = {
      val result = Promise[String]
      val result_future = result.future
      ws.sendBinary(bundle.toDtnWsJson).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get)))
      Await.result(result_future, Duration.Inf)
    }

    // select json communication
    println(command("/json"))

    def registerEndpointAndSubscribe(service: String): Unit = {
      // register the endpoint on the DTN daemon
      uget(uri"$http_api/register?$service")
      registeredServices = service :: registeredServices

      // subscribe to the registered endpoint with out websocket
      println(command(s"/subscribe $service"))
    }

    def disconnect(): Unit = {
      for (service <- registeredServices) {
        uget(uri"$http_api/unregister?$service")
      }
      registeredServices = List()

      ws.close()
    }
  }

  /*
  More sophisticated websocket wrapper class for scala3, where java.lang.Thread is supported.
  The Interface returns Futures when it is useful and serializes requests internally via a queue with a worker thread.
  */
  class WS {
    private var registeredServices: List[String] = List()
    
    private val ws: WebSocket[Future] = Await.result(backend.send(basicRequest.get(uri"$ws_url").response(asWebSocketAlwaysUnsafe)), Duration.Inf).body

    private val requestQueue: ConcurrentLinkedQueue[(Future[Any], () => Unit)] = ConcurrentLinkedQueue[(Future[Any], () => Unit)]

    // this background thread serializes concurrent requests to the websocket
    private var backendThreadKeepRunning: Boolean = true
    private val backendThread = new Thread {
      override def run(): Unit = {
        while (backendThreadKeepRunning) {
          val tuple = requestQueue.poll()
          if (tuple != null) {
            tuple(1)()  // execute lambda
            Await.ready(tuple(0), Duration.Inf)  // wait until lambda has set its promise
          }
        }
      }
    }
    backendThread.start()

    private def command(text: String): String = {
      val result = Promise[String]
      val result_future = result.future
      requestQueue.add(result_future, () => ws.sendText(text).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get))))
      Await.result(result_future, Duration.Inf)
    }

    def receiveBundle(): Future[Bundle] = {
      val result = Promise[Bundle]
      val result_future = result.future
      requestQueue.add(result_future, () => ws.receiveBinary(true).onComplete(response => result.success((Bundle.createFromDtnWsJson(response.get)))))
      result_future
    }

    def sendBundle(bundle: Bundle): Future[String] = {
      val result = Promise[String]
      val result_future = result.future
      requestQueue.add(result_future, () => ws.sendBinary(bundle.toDtnWsJson).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get))))
      result_future
    }

    // select json communication
    println(command("/json"))

    def registerEndpointAndSubscribe(service: String): Unit = {
      // register the endpoint on the DTN daemon
      uget(uri"$http_api/register?$service")
      registeredServices = service :: registeredServices

      // subscribe to the registered endpoint with out websocket
      println(command(s"/subscribe $service"))
    }

    def disconnect(): Unit = {
      for (service <- registeredServices) {
        uget(uri"$http_api/unregister?$service")
      }
      registeredServices = List()

      ws.close()
      
      while(!requestQueue.isEmpty) {}
      backendThreadKeepRunning = false
    }
  }
  def createWS: WS = WS()
  def createWS_JsCompat: WS_JsCompat = WS_JsCompat()
}

case class SendBundle(src: String, dst: String, data: String, delivery_notification: Boolean, lifetime: Long)
case class ReceivedBundle(bid: String, src: String, dst: String, data: String)
// automatic JSON encoder/decoder for case classes
given JsonValueCodec[ReceivedBundle] = JsonCodecMaker.make
given JsonValueCodec[SendBundle] = JsonCodecMaker.make

case class Bundle(bid: String = "", src: String = "", dst: String = "", data: String = "", delivery_notification: Boolean = false, lifetime: Long = 3600 * 24 * 1000) {
  def toDtnWsJson: Array[Byte] = {
    val bundle: SendBundle = SendBundle(src = src, dst = dst, data = data, delivery_notification = delivery_notification, lifetime = lifetime)
    writeToArray(bundle)
  }
  
  override def toString: String = s"Bundle(bid: $bid, src: $src, dst: $dst, data: $data, delivery_notification: $delivery_notification, lifetime: $lifetime)"

  def getDataAsUTF8Text: String = String(Base64.getDecoder.decode(data), StandardCharsets.UTF_8)

  def getDataAsBytes: Array[Byte] = Base64.getDecoder.decode(data)
}
object Bundle {
  def createWithUTF8TextAsData(src: String, dst: String, strng: String, delivery_notification: Boolean = false, lifetime: Long = 3600 * 24 * 1000): Bundle = {
    val data = Base64.getEncoder.encodeToString(strng.getBytes(StandardCharsets.UTF_8))
    Bundle(src = src, dst = dst, data = data, delivery_notification = delivery_notification, lifetime = lifetime)
  }

  def createWithBytesAsData(src: String, dst: String, bytes: Array[Byte], delivery_notification: Boolean = false, lifetime: Long = 3600 * 24 * 1000): Bundle = {
    val data = Base64.getEncoder.encodeToString(bytes)
    Bundle(src = src, dst = dst, data = data, delivery_notification = delivery_notification, lifetime = lifetime)
  }

  def createFromDtnWsJson(input: Array[Byte]): Bundle = {
    val bundle: ReceivedBundle = readFromArray[ReceivedBundle](input)
    Bundle(bid = bundle.bid, src = bundle.src, dst = bundle.dst, data = bundle.data)
  }
}


@main def run(): Unit = {
  if (true) {
    val conn = Client.createWS

    println(s"connected to node: ${Client.nodeId}")

    if (Client.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

    val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

    conn.registerEndpointAndSubscribe(cRDTGroupEndpoint)

    val bundle = Bundle.createWithUTF8TextAsData(
      src = cRDTGroupEndpoint,
      dst = cRDTGroupEndpoint,
      strng = "Hello World",
      lifetime = 3600 * 24 * 1000,
      delivery_notification = false
    )
    println(s"start sending bundle with text: ${bundle.getDataAsUTF8Text}")
    conn.sendBundle(bundle).onComplete(response => println(s"finished sending bundle, response: ${response.get}"))

    println(s"start receiving bundle")
    conn.receiveBundle().onComplete(response => {
      println(s"received bundle with text: ${response.get.getDataAsUTF8Text}")
      conn.disconnect()
    })
  } else {
    val conn = Client.createWS_JsCompat

    println(s"connected to node: ${Client.nodeId}")

    if (Client.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

    val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

    conn.registerEndpointAndSubscribe(cRDTGroupEndpoint)

    val bundle = Bundle.createWithUTF8TextAsData(
      src = cRDTGroupEndpoint,
      dst = cRDTGroupEndpoint,
      strng = "Hello World",
      lifetime = 3600 * 24 * 1000,
      delivery_notification = false
    )
    println(s"start sending bundle with text: ${bundle.getDataAsUTF8Text}")
    val responseStr: String = conn.sendBundle(bundle)
    println(s"finished sending bundle, response: ${responseStr}")

    println(s"start receiving bundle")
    val responseBundle: Bundle = conn.receiveBundle()
    println(s"received bundle with text: ${responseBundle.getDataAsUTF8Text}")
    conn.disconnect()
  }
}
