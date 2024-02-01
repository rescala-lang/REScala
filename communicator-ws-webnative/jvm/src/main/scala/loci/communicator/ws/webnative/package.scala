package loci
package communicator
package ws

package object webnative {
  private[webnative] def unavailable = sys.error("Web native WebSocket communicator only available in JS")
}
