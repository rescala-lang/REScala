package reader.data

import collection.mutable.Map
import scala.events._
import scala.events.behaviour._

/**
 * The FeedStore stores RSSChannels and RSSItems.
 * More specifically, it stores the relation between an RSS Item and its channel
 * to enable clients to ask e.g. for all items stored related to a specific channel.
 */
class FeedStore {
  private var channelToItems: Map[RSSChannel,Set[RSSItem]] = Map()

  val channelsChanged            = new ImperativeEvent[List[RSSChannel]]
//  val channelChanged = Signal{
//    List[RSSChannel]
//  }
  val itemAdded                  = new ImperativeEvent[RSSItem]
  val contentChange: Event[Unit] = channelsChanged.dropParam || itemAdded.dropParam

  private var channelsField: List[RSSChannel] = channelToItems.keys.toList
  def channels: List[RSSChannel] = channelsField

  def addChannel(channel: RSSChannel) {
    if (channelToItems.contains(channel)) return

    channelToItems += channel -> Set()
    val newValue = channelToItems.keys.toList
    channelsField = newValue
    channelsChanged(newValue)
  }

  /*
  * Check whether the item:
  *   - has a source channel
  *   - the channel of the item is being tracked (in the map)
  *   - the item is not yet stored
  * if all of these hold, return true
  */
  private def addItemAllowed(item: RSSItem): Boolean = {
    val res = for { channel <- item.srcChannel
                    items   <- channelToItems.get(channel)
                    if (!items.contains(item))
                  } yield Some(true)
    res.isDefined
  }

  def addItem(item: RSSItem) {
    if (!addItemAllowed(item)) return

    val channel = item.srcChannel.get
    channelToItems += channel -> { channelToItems(channel) + item }
    itemAdded(item)
  }

  def itemsFor(channel: RSSChannel): Option[Set[RSSItem]] = {
    channelToItems.get(channel)
  }

}
