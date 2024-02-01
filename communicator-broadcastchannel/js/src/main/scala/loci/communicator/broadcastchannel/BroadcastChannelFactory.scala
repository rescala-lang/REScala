package loci
package communicator
package broadcastchannel

private object BroadcastChannelSetupParser
    extends ConnectionSetupParser
    with SimpleConnectionSetupProperties {
  val self: BroadcastChannel.type = BroadcastChannel

  def properties =
    BroadcastChannel.Properties()
}

trait BroadcastChannelSetupFactory extends ConnectionSetupFactory.Implementation[BroadcastChannel] {
  val self: BroadcastChannel.type = BroadcastChannel

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): BroadcastChannel.Properties =
    BroadcastChannelSetupParser.properties

  protected def listener(
      url: String, scheme: String, location: String, properties: BroadcastChannel.Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: BroadcastChannel.Properties) =
    Some(BroadcastChannel(url, properties))
}
