package loci
package communicator

package object broadcastchannel {
  private[broadcastchannel] def unavailable = sys.error("BroadcastChannel communicator only available in JS")
}
