package loci
package communicator
package ws

package object jetty {
  private[jetty] def unavailable = sys.error("Jetty WebSocket communicator only available on the JVM")
}
