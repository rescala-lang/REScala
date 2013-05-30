package reader.data

import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.events.behaviour.Var

/**
 * The FeedStore stores RSSChannels and RSSItems.
 * More specifically, it stores the relation between an RSS Item and its channel
 * to enable clients to ask e.g. for all items stored related to a specific channel.
 */
class FeedStore {
  private val channelToItems = Var(Map.empty[RSSChannel, Var[Set[RSSItem]]])
  
  val channels = Signal {
    channelToItems() map { case (channel, items) => (channel, Signal { items() }) } }
  
  final val itemAdded = new ImperativeEvent[RSSItem]
  
  def addChannel(channel: RSSChannel) =
    channelToItems() = channelToItems.getValue + (channel -> Var(Set.empty))
  
  /*
   * Check whether the item:
   *   - has a source channel
   *   - the channel of the item is being tracked (in the map)
   *   - the item is not yet stored
   * if all of these hold, return true
   */
  private def addItemAllowed(item: RSSItem): Boolean = {
    val res = for { channel <- item.srcChannel
                    items   <- channelToItems.getValue get channel
                    if (!(items.getValue contains item))
                  } yield Some(true)
    res.isDefined
  }
  
  def addItem(item: RSSItem) =
    if (addItemAllowed(item)) {
      val channel = item.srcChannel.get
      channelToItems.getValue(channel)() += item
      itemAdded(item)
    }
}
