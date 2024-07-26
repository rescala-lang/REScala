package dtn

import io.bullet.borer.Cbor
import rdts.time.Dots

class MonitoringServer(server: TCPReadonlyServer) {
  def run(): Unit = {
    try {
      while true do {
        val (connection, data)          = server.queue.take()
        val received: MonitoringMessage = Cbor.decode(data).to[MonitoringMessage].value
      }
    } catch {
      case e: Exception =>
        println(s"monitoring server ran into exception: $e")
        server.stop()
    }
  }

}
object MonitoringServer {
  def apply(interface: String, port: Int): MonitoringServer = {
    val server = TCPReadonlyServer(interface, port)
    server.start()
    println(s"monitoring server started under $interface:$port")
    new MonitoringServer(server)
  }
}

class MonitoringClient(connection: TCPConnection) extends MonitoringClientInterface {
  override def send(message: MonitoringMessage): Unit = {
    connection.send(Cbor.encode[MonitoringMessage](message).toByteArray)
  }
}
object MonitoringClient {
  def apply(host: String, port: Int): MonitoringClient = {
    new MonitoringClient(TCPConnection(host, port))
  }
}
