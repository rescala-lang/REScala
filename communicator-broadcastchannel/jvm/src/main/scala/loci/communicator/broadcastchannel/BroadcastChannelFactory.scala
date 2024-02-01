package loci
package communicator
package broadcastchannel

private object BroadcastChannelSetupParser
    extends ConnectionSetupParser
    with SimpleConnectionSetupProperties {
  val self: BroadcastChannel.type = unavailable

  def properties = unavailable
}

trait BroadcastChannelSetupFactory extends ConnectionSetupFactory.Implementation[BroadcastChannel] {
  val self: BroadcastChannel.type = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties): BroadcastChannel.Properties = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: BroadcastChannel.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: BroadcastChannel.Properties) = unavailable
}
