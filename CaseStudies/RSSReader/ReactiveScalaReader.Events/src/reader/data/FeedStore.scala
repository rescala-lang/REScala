package reader.data

import scala.annotation.migration
import scala.collection.mutable.Map
import rescala.events._


/**
 * The FeedStore stores RSSChannels and RSSItems.
 * More specifically, it stores the relation between an RSS Item and its channel
 * to enable clients to ask e.g. for all items stored related to a specific channel.
 */
class FeedStore {
  private val channelToItems = Map.empty[RSSChannel, Set[RSSItem]]
  
  val channelsChanged = new ImperativeEvent[List[RSSChannel]] //#EVT
  val itemAdded = new ImperativeEvent[RSSItem] //#EVT
  val contentChanged: Event[Unit] = channelsChanged.dropParam || itemAdded.dropParam //#EVT //#EF //#EF //#EF
  
  def channels = channelToItems.keys.toList
  
  def addChannel(channel: RSSChannel) =
    if (!(channelToItems contains channel)) {
      channelToItems += channel -> Set.empty
      val newValue = channelToItems.keys.toList
      channelsChanged(newValue)
    }
  
  /*
   * Check whether the item:
   *   - has a source channel
   *   - the channel of the item is being tracked (in the map)
   *   - the item is not yet stored
   * if all of these hold, return true
   */
  private def addItemAllowed(item: RSSItem): Boolean =
    (for { channel <- item.srcChannel
           items   <- channelToItems get channel
           if (!(items contains item))
         } yield ()).isDefined
  
  def addItem(item: RSSItem) =
    if (addItemAllowed(item)) {
      val channel = item.srcChannel.get
      channelToItems += channel -> { channelToItems(channel) + item }
      itemAdded(item)
    }
  
  def itemsFor(channel: RSSChannel) =
    channelToItems get channel
}
