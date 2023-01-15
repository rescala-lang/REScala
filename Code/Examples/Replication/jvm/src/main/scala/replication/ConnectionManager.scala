package replication

import loci.communicator.tcp.TCP
import loci.registry.Registry

class ConnectionManager(registry: Registry) {

  def listenTcp(port: Int): Unit =
    registry.listen(TCP(port))

  def connectTcp(ip: String, port: Int) =
    registry.connect(TCP(ip, port))

}
