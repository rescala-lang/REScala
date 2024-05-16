package dtn

import kofre.time.Dots
import java.util.concurrent.LinkedBlockingQueue
import io.bullet.borer.Cbor

class DotsConvergenceChecker(server: TCPServer) {
  var state: Map[TCPConnection, Dots] = Map()

  def run(): Unit = {
    try {
      while(true) {
        var newStateReceived = false

        server.connections.forEach{
          case (connection, (senderQueue, receiverQueue)) => 
            val data = receiverQueue.poll()

            if (data != null) {
              newStateReceived = true

              state += (connection -> Cbor.decode(data).to[Dots].value)
            }
        }

        if (newStateReceived) {
          if (Set(state.values).size == 1) {
            println("states are equal")
          } else {
            println("states not equal")
          }
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
    val server = TCPServer(port, interface)
    server.start()
    new DotsConvergenceChecker(server)
  }
}
