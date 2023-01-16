package replication

import loci.communicator.tcp.TCP
import loci.registry.Registry
import replication.server.JettyServer

class ConnectionManager(registry: Registry) {

  val server = new JettyServer(None, "", registry, "0")

  def listenTcp(port: Int): Unit =
    registry.listen(TCP(port))

  def connectTcp(ip: String, port: Int) =
    registry.connect(TCP(ip, port))

  def startWebserver(port: Int) =
    server.start(port)

}
