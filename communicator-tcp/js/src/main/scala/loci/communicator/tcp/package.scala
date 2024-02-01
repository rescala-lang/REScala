package loci
package communicator

package object tcp {
  private[tcp] def unavailable = sys.error("TCP communicator only available on the JVM")
}
