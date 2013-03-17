package reader.gui

import reader.data._

import scala.swing._

trait ContentMediator {
  def mediate( channelList: EventListView[RSSChannel]
             , itemList: EventListView[RSSItem]
             , renderArea: RssItemRenderPane
             , store: FeedStore): Unit
}

/**
* Coordinates the containers in the gui:
*   - the channel list
*   - the item list
*   - the render area for feeds
*   - the store for the feeds
*/
object SyncAll extends ContentMediator {
  def mediate( channelList: EventListView[RSSChannel]
             , itemList: EventListView[RSSItem]
             , renderArea: RssItemRenderPane
             , store: FeedStore) {

    updateItemsListOnAddition
    displayItemsForSelectedChannel
    renderSelectedItem

    def updateItemsListOnAddition {
      store.itemAdded += { item =>
        for {
          selected <- channelList.selectedItem
          src <- item.srcChannel
          if (src == selected)
        } displayChannelsItems(src)
      }
    }

    def displayItemsForSelectedChannel {
      channelList.selectedItemChanged += { maybeChannel: Option[RSSChannel] =>
        maybeChannel.foreach { channel => displayChannelsItems(channel) }
      }
    }

    def renderSelectedItem {
      itemList.selectedItemChanged += { maybeItem =>
        maybeItem.foreach { item =>
          renderArea.content_=(item)
        }
      }
    }

    def displayChannelsItems(channel: RSSChannel) = {
      store.itemsFor(channel).foreach {  items =>
        itemList.listData = items.toSeq.sorted
        itemList.repaint
      }
    }
  }
}
