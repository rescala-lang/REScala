package ex201x.reswingexamples.reader.data

import reactives.default.*

/** The FeedStore stores RSSChannels and RSSItems.
  * More specifically, it stores the relation between an RSS Item and its channel
  * to enable clients to ask e.g. for all items stored related to a specific channel.
  */
class FeedStore(
    val addChannel: Event[RSSChannel],
    val addItem: Event[RSSItem]
) {

  val channels = addChannel.fold(Map.empty[RSSChannel, Signal[Set[RSSItem]]]) { (map, channel) => // #SIG //#IF
    map + (channel ->
    (addItem && (_.srcChannel.isDefined) && (_.srcChannel.get == channel)). // #EF //#EF
    fold(Set.empty[RSSItem])(_ + _))                                        // #IF
  }

  val itemAdded: Event[RSSItem] = Event.dynamic {
    addItem.value.filter { item => // #EVT //#EF
      val contains =
        for
          channel <- item.srcChannel
          items   <- channels.value get channel
          if !(items.value contains item)
        yield ()

      contains.isDefined
    }
  }
}
