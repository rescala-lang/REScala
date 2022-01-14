
package intermediaries_demo

import encrdt.sync.client_server.UntrustedReplicaWebSocketServer

object UntrustedReplicaApp extends App {
  val server = new UntrustedReplicaWebSocketServer()
  server.start()
  sys.addShutdownHook(server.stop())
  println("Untrusted replica listening on: untrusted@" + server.uri)
}
