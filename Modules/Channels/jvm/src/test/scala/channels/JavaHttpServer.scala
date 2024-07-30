package channels

import channels.JavaHttp.SSEServer
import channels.MessageBuffer.given_Conversion_String_MessageBuffer
import com.sun.net.httpserver.HttpServer

import java.net.InetSocketAddress
import scala.util.{Failure, Success}

object JavaHttpServerTest {

  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(InetSocketAddress(58080), 0)

    val conn = SSEServer(handler => server.createContext("/channel", handler))

    conn.prepare(inc => msg => println(msg)).run(using Abort()):
      case Failure(ex) => ex.printStackTrace()
      case Success(conn) =>
        println(s"received connection, replying")
        conn.send("yay!".convert).run(using Abort())(res => println(res))

    server.start()

  }
}
