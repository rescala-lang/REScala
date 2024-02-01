package loci
package communicator
package broadcastchannel

import scala.annotation.compileTimeOnly

trait BroadcastChannel
    extends Protocol
    with SetupInfo
    with SecurityInfo
    with SymmetryInfo with Bidirectional {

  val name: String

  val authenticated: Boolean = false
  val encrypted: Boolean = false
  val integrityProtected: Boolean = false

  override def toString = s"BroadcastChannel($name)"
}

@compileTimeOnly("BroadcastChannel communicator only available in JS")
object BroadcastChannel extends BroadcastChannelSetupFactory {
  def unapply(broadcastChannel: BroadcastChannel) = Some((broadcastChannel.name))

  case class Properties()

  val schemes: Seq[String] = unavailable

  def apply(name: String): Connector[BroadcastChannel] = unavailable

  def apply(name: String, properties: Properties): Connector[BroadcastChannel] = unavailable
}
