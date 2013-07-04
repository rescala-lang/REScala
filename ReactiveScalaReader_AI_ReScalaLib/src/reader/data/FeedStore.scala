package reader.data

import react.Signal
import react.events.Event

/**
 * The FeedStore stores RSSChannels and RSSItems.
 * More specifically, it stores the relation between an RSS Item and its channel
 * to enable clients to ask e.g. for all items stored related to a specific channel.
 */
class FeedStore (
    val addChannel: Event[RSSChannel],
    val addItem: Event[RSSItem]) {
  
  val channels = addChannel.fold(Map.empty[RSSChannel, Signal[Set[RSSItem]]]) { (map, channel) => //#SIG //#IF
	  map + (channel ->
	    (addItem && (_.srcChannel.isDefined) && (_.srcChannel.get == channel)). //#EF //#EF
	      fold(Set.empty[RSSItem])(_ + _)) //#IF
    }
  
  val itemAdded: Event[RSSItem] = addItem && { item => //#EVT //#EF
    (for { channel <- item.srcChannel
           items   <- channels.getValue get channel
           if (!(items.getValue contains item))
         } yield ()).isDefined
  }
}
