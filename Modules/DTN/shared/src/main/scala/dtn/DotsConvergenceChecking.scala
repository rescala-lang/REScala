package dtn

import io.bullet.borer.Cbor
import rdts.time.Dots

class DotsConvergenceChecker(server: TCPReadonlyServer) {
  var state: Map[TCPConnection, Dots] = Map()

  def run(): Unit = {
    try {
      while true do {
        val (connection, data) = server.queue.take()

        val old_dots = state.getOrElse(connection, Dots.empty)

        state += (connection -> old_dots.merge(Cbor.decode(data).to[Dots].value))

        println(
          s"states are equal? ${Set.from(state.values).size == 1} ${state.values.drop(1).forall(d => d == state.values.head)}"
        )
      }
    } catch {
      case e: java.lang.Exception =>
        println(s"checker ran into exception: $e")
        server.stop()
    }
  }

}
object DotsConvergenceChecker {
  def apply(interface: String, port: Int): DotsConvergenceChecker = {
    val server = TCPReadonlyServer(interface, port)
    server.start()
    println(s"convergence checker started under $interface:$port")
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
