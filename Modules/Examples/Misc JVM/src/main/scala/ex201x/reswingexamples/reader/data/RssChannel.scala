package ex201x.reswingexamples.reader.data

import java.net.URL
import java.util.Date

/** Represents an RSSChannel with its metadata
  *
  * @param title       - name of the channel
  * @param link        - URL to the website corresponding to the channel
  * @param description - phrase or sentence describing the channel
  *
  * optional:
  *
  * @param pubDate     - publication date for the content of the channel
  * @param source      - URL to the rss feed this channel was obtained from
  */
case class RSSChannel(title: String, link: Option[URL], description: String, pubDate: Option[Date], source: Option[URL])
    extends Ordered[RSSChannel] {
  def compare(that: RSSChannel) = {
    val result =
      for
        thisDate <- this.pubDate
        thatDate <- that.pubDate
      yield { thisDate `compareTo` thatDate }

    result getOrElse 0
  }
}

object RSSChannel {
  def changeSource(channel: RSSChannel, src: Option[URL]) =
    channel match {
      case RSSChannel(title, link, desc, date, _) => RSSChannel(title, link, desc, date, src)
    }
}
