package loci
package transmitter

trait AbstractionRef {
  def remote: RemoteRef
  def channel: Channel
  def derive(name: String): AbstractionRef
}
