package reader.gui

import reader.data.FeedStore
import reader.data.RSSChannel
import reader.data.RSSItem
import scala.swing.ListView
import rescala._

trait ContentMediator {
  def mediate(channelList: ReListViewEx[RSSChannel],
              itemList: ReListViewEx[RSSItem],
              renderArea: RssItemRenderPane,
              store: FeedStore): Unit
}

/**
* Coordinates the containers in the gui:
*   - the channel list
*   - the item list
*   - the render area for feeds
*   - the store for the feeds
*/
object SyncAll extends ContentMediator {
  def mediate(channelList: ReListViewEx[RSSChannel],
              itemList: ReListViewEx[RSSItem],
              renderArea: RssItemRenderPane,
              store: FeedStore): Unit = {
    store.itemAdded += { item =>
      for {
        selected <- channelList.selectedItem
        src <- item.srcChannel
        if (src == selected)
      } displayChannelsItems(src)
    }

    channelList.selectedItemChanged += { maybeChannel: Option[RSSChannel] =>
      for (channel <- maybeChannel) displayChannelsItems(channel)
    }

    itemList.selectedItemChanged += { maybeItem =>
      for (item <- maybeItem) renderArea.renderItem(item)
    }

    def displayChannelsItems(channel: RSSChannel) = {
      store itemsFor channel foreach { items =>
        val listView = itemList: ListView[RSSItem]
        listView.listData = items.toSeq.sorted
        listView.repaint
      }
    }
  }
}
