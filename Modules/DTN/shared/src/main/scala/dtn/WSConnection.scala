package dtn

import io.bullet.borer.{Cbor, Json}
import sttp.capabilities.WebSockets
import sttp.client4.*
import sttp.client4.ws.async.*
import sttp.model.Uri
import sttp.ws.WebSocket
import sttp.ws.WebSocketFrame.{Binary, Ping, Pong, Text}

import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import java.time.ZonedDateTime

class WSConnection(ws: WebSocket[Future]) {
  val backend: GenericBackend[Future, WebSockets] = CompatCode.backend

  def command(text: String): Unit = {
    ws.sendText(text).printError()
  }

  def receiveWholeMessage(): Future[String | Array[Byte]] = {
    def combineFragments(f1: String | Array[Byte], f2: String | Array[Byte]): String | Array[Byte] = {
      f1 match {
        case s: String => f2 match {
            case s2: String      => s + s2
            case b2: Array[Byte] => throw Exception("cannot combine String and Array[Byte] fragment")
          }
        case b: Array[Byte] => f2 match {
            case s2: String      => throw Exception("cannot combine String and Array[Byte] fragment")
            case b2: Array[Byte] => b ++ b2
          }
      }
    }

    ws.receive().flatMap {
      case Binary(payload: Array[Byte], finalFragment: Boolean, rsv: Option[Int]) => {
        if finalFragment then {
          Future(payload)
        } else {
          receiveWholeMessage().map(payload2 => combineFragments(payload, payload2))
        }
      }
      case Text(payload: String, finalFragment: Boolean, rsv: Option[Int]) => {
        if finalFragment then {
          Future(payload)
        } else {
          receiveWholeMessage().map(payload2 => combineFragments(payload, payload2))
        }
      }
      case Ping(payload: Array[Byte]) => {
        // js FetchBackend and jvm HttpClientFutureBackend seem to answer these automatically
        // HttpClientFutureBackend forwards these messages to us, FetchBackend does not
        // in either case, no actions are required
        receiveWholeMessage()
      }
      case Pong(payload: Array[Byte]) => {
        receiveWholeMessage()
      }
      case _ => {
        println("unknown received data type. ignoring.")
        receiveWholeMessage()
      }
    }
  }

  def sendBinary(payload: Array[Byte]): Future[Unit] = ws.sendBinary(payload)

  def sendText(payload: String): Future[Unit] = ws.sendText(payload)

  def close(): Future[Unit] = ws.close()
}
object WSConnection {
  def apply(url: Uri): Future[WSConnection] = {
    CompatCode.backend.send(basicRequest.get(url).response(asWebSocketAlwaysUnsafe)).map(x => new WSConnection(x.body))
  }
}

class WSEndpointClient(host: String, port: Int, connection: WSConnection, val nodeId: String) {
  protected var registeredServices: List[String] = List()

  def receiveBundle(): Future[Bundle] = {
    /*
    We have a problem:
    The "command() and sendBundle() functions" trigger a string response by the dtn7-rs indicating its success.
    But, because of the Futures, if we wait on command() and receiveBundle(), we could wait concurrently, possibly returning the wrong result to the wrong function.
    SOLUTION: we only receive in this function, throwing an error if the command response indicates "non-successful".
    Not ideal, but should be sufficient for now.
     */

    connection.receiveWholeMessage().flatMap {
      case s: String => {
        // string responses should always start with 200
        // examples: 200 tx mode: JSON, 200 subscribed, 200 Sent bundle dtn://global/~crdt/app1-764256828302-0 with 11 bytes
        // we throw an Exception if this is not the case
        println(s"received command response: $s")
        if !s.startsWith("200") then
          println(
            s"dtn ws command response indicated 'not successfull', further interaction with the ws will likely fail: $s"
          )
        receiveBundle()
      }
      case b: Array[Byte] => {
        Future(Cbor.decode(b).to[Bundle].value)
      }
    }
  }

  def sendBundle(bundle: Bundle): Future[Unit] = {
    println(s"starting to send bundle at time: ${ZonedDateTime.now()}")
    connection.sendBinary(Cbor.encode(bundle).toByteArray).map(u => {
      println(s"sent bundle at time: ${ZonedDateTime.now()}")
      u
    })
  }

  def registerEndpointAndSubscribe(service: String): Future[WSEndpointClient] = {
    // register the endpoint on the DTN daemon
    CompatCode.uget(uri"http://${host}:${port}/register?${service}").map(_ => {
      registeredServices = service :: registeredServices
      // subscribe to the registered endpoint with our websocket
      connection.command(s"/subscribe $service")
      this
    })
  }

  def disconnect(): Future[Unit] = {
    // currently unused method so we do not unregister atm todo: check when we actually want to unregister
    registeredServices.foreach(service => CompatCode.uget(uri"http://${host}:${port}/unregister?${service}"))
    registeredServices = List()
    connection.close()
    // todo: do I need to make sure each uget is done before leaving this method?
  }
}
object WSEndpointClient {
  def apply(host: String, port: Int): Future[WSEndpointClient] = {
    var nodeId: Option[String] = None

    CompatCode.uget(uri"http://${host}:${port}/status/nodeid")
      .flatMap(nId => {
        println(s"connected to DTN node: $nId"); // like: "dtn://node3000/"
        nodeId = Option(nId)
        if nId.startsWith("ipn") then {
          println("DTN mode IPN is unsupported by this client. throwing");
          throw Exception("DTN mode IPN is unsupported by this client")
        }
        WSConnection(uri"ws://${host}:${port}/ws")
      })
      .map(connection => {
        connection.command("/bundle")
        new WSEndpointClient(host, port, connection, nodeId.get)
      })
  }
}

class WSEroutingClient(host: String, port: Int, connection: WSConnection, val nodeId: String) {
  def receivePacket(): Future[Packet] = {
    connection.receiveWholeMessage().flatMap {
      case s: String => {
        Future(Json.decode(s.getBytes(StandardCharsets.UTF_8)).to[Packet].value)
      }
      case b: Array[Byte] => {
        println("received bytes on external routing ws, but shouldn't have. ignoring")
        receivePacket()
      }
    }
  }

  def sendPacket(packet: Packet): Future[Unit] = {
    connection.sendText(Json.encode(packet).toUtf8String)
  }
}
object WSEroutingClient {
  def apply(host: String, port: Int): Future[WSEroutingClient] = {
    var nodeId: Option[String] = None

    CompatCode.uget(uri"http://${host}:${port}/status/nodeid")
      .flatMap(nId => {
        println(s"connected to DTN node: $nId");
        nodeId = Option(nId)
        if nId.startsWith("ipn") then {
          println("DTN mode IPN is unsupported by this client. throwing");
          throw Exception("DTN mode IPN is unsupported by this client")
        }
        WSConnection(uri"ws://${host}:${port}/ws/erouting")
      })
      .map(connection => new WSEroutingClient(host, port, connection, nodeId.get))
  }
}
