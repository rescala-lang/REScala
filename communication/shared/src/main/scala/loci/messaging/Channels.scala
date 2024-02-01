package loci
package messaging

import transmitter.RemoteRef

import java.util.concurrent.ConcurrentHashMap

object Channels {
  trait Channel {
    val name: String
    val remote: RemoteRef
  }
}

class Channels[C <: Channels.Channel, -R <: RemoteRef](
    createChannel: (String, String, R) => C,
    closeChannel: (C, Boolean) => Unit) {

  private val channels = new ConcurrentHashMap[(String, RemoteRef), C]

  def obtain(name: String, anchorDefault: String, remote: R): C = {
    val channelId = name -> remote
    val channel = createChannel(name, anchorDefault, remote)
    if (remote.connected) {
      val obtainedChannel =
        Option(channels.putIfAbsent(channelId, channel)) getOrElse channel

      if (!remote.connected)
        channels.remove(obtainedChannel)

      obtainedChannel
    }
    else
      channel
  }

  def get(name: String, remote: R): Option[C] = {
    val channelId = name -> remote
    if (remote.connected)
      Option(channels get channelId)
    else
      None
  }

  def close(channel: C, notifyRemote: Boolean): Unit = {
    val channelId = (channel.name, channel.remote)
    Option(channels.remove(channelId)) foreach { closeChannel(_, notifyRemote) }
  }

  def close(remote: R): Unit = {
    val iterator = channels.keySet.iterator
    while (iterator.hasNext)
      iterator.next() match {
        case id @ (_, `remote`) =>
          Option(channels.remove(id)) foreach { closeChannel(_, false) }
        case _ =>
      }
  }

  def isOpen(channel: C): Boolean = {
    val channelId = (channel.name, channel.remote)
    channel == (channels get channelId)
  }
}
