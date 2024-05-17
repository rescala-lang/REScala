package dtn

import kofre.time.Dots
import java.util.concurrent.LinkedBlockingQueue
import io.bullet.borer.Cbor

class DotsConvergenceChecker(server: TCPReadonlyServer) {
  var state: Map[TCPConnection, Dots] = Map()

  def run(): Unit = {
    try {
      while(true) {
        val (connection, data) = server.queue.take()

        state += (connection -> Cbor.decode(data).to[Dots].value)

        if (Set(state.values).size == 1) {
          println("states are equal")
        } else {
          println("states not equal")
        }
      }
    } catch {
      case e: java.lang.Exception => {
        println(s"checker ran into exception: $e")
        server.stop()
      }
    }
  }
  
}
object DotsConvergenceChecker {
  def apply(port: Int, interface: String): DotsConvergenceChecker = {
    val server = TCPReadonlyServer(port, interface)
    server.start()
    new DotsConvergenceChecker(server)
  }
}


class DotsConvergenceClient(connection: TCPConnection) {
  def send(dots: Dots): Unit = {
    connection.send(Cbor.encode[Dots](dots).toByteArray)
  }
}
object DotsConvergenceClient {
  def apply(host: String, port: Int): DotsConvergenceClient = {
    new DotsConvergenceClient(TCPConnection(host, port))
  }
}
