package loci
package communicator
package broadcastchannel

import scala.util.{Success, Try}

private sealed trait BroadcastChannelProtocolFactory[P <: BroadcastChannel] {
  def make(name: String, setup: ConnectionSetup[P]): Try[P]
}

private object BroadcastChannelProtocolFactory {
  locally(BroadcastChannelProtocolFactory)

  implicit object broadcastchannel extends BroadcastChannelProtocolFactory[BroadcastChannel] {
    def make(name: String, setup: ConnectionSetup[BroadcastChannel]): Try[BroadcastChannel] =
      Success(construct(name, setup))
  }

  private def construct(_name: String, _setup: ConnectionSetup[BroadcastChannel]) =
    new BroadcastChannel {
      val name = _name;
      val setup = _setup;
    }
}
