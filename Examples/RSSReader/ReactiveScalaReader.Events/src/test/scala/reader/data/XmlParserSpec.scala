package reader.data

import java.net.URL
import java.util.Calendar

import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import reader.EventShouldFireWrapper.convertToEventShouldFireWrapper
import reader.XMLFixtures.completeRSS2Items
import reader.XMLFixtures.corruptDateItem
import reader.XMLFixtures.missingLinkChannel
import reader.XMLFixtures.missingLinkItem
import reader.XMLFixtures.simpleChannel
import reader.XMLFixtures.simpleItem
import reader.common.implicits.stringToUrl
import rescala._

class XmlParserSpec extends FlatSpec with Matchers with BeforeAndAfter {
  var parser: XmlParser = _

  before {
    parser = new XmlParser
  }

  "The XML-Parser" should "parse channels from xml" in {
    val channel = parser.parseChannelWithoutURL(simpleChannel)
    channel.get.title should equal("feedTest feed")
  }

  it should "parse a channel with url" in {
    val url: URL = "http://www.test.de"
    val channel = parser.parseChannelWithURL(simpleChannel, url)

    channel == None should equal(false)

    channel.get.source.get should equal(url)
  }

  it should "set None as the link if not found in xml" in {
    val channel = parser.parseChannelWithoutURL(missingLinkChannel).get

    channel.link should equal(None)
  }

  it should "parse an item from xml" in {
    val item = parser.parseItem(simpleItem)
    item.get.title should equal("Test title")
  }

  it should "parse the date from an item" in {
    val item = parser.parseItem(simpleItem).get

    val date = item.pubDate.get
    val cal = Calendar.getInstance()
    cal.setTime(date)
    cal.setFirstDayOfWeek(Calendar.TUESDAY)
    cal.get(Calendar.DAY_OF_WEEK) should equal(Calendar.SATURDAY)
    cal.get(Calendar.MONTH) should equal(Calendar.DECEMBER)
  }

  it should "set the date to None if the format can not be parsed" in {
    val item = parser.parseItem(corruptDateItem).get

    item.pubDate should equal(None)
  }

  it should "set the link to None if it is missing in the xml" in {
    val item = parser.parseItem(corruptDateItem).get

    item.pubDate should equal(None)
  }

  it should "set None for the link if it is not present in the tag" in {
    val item = parser.parseItem(missingLinkItem).get.link should equal(None)
  }

  it should "fire an event after parsing a channel" in {
    parser.channelParsed shouldFireIn {
      parser.parseChannelWithoutURL(simpleChannel)
    }

    parser.channelParsed shouldFireIn {
      parser.parseChannelWithURL(simpleChannel, new URL("http://www.what.ever"))
    }
  }

  it should "fire an event after parsing an item" in {
    parser.itemParsed shouldFireIn {
      parser.parseItem(simpleItem)
    }
  }

  it should "parse a channel and it's items" in {
    val res = parser.parseRSS(completeRSS2Items,new URL("http://www.test.de"))

    res should be ('defined)

    val (channel,items) = res.get

    channel.title should equal("feedTest feed")
    items should have size 2
    items map(_.title) should equal (List("Test title","Simple Item 2"))
    items foreach { _ should have ('srcChannel (Some(channel))) }
  }

  it should "fire itemParsed with RSSItems that DO have a src channel after parseRSS" in {
    parser.itemParsed += { _.srcChannel should be ('defined) }

    parser.parseRSS(completeRSS2Items,new URL("http://www.test.de"))
  }

  it should "fire channelParsed after parseRSS" in {
    parser.channelParsed shouldFireIn {
      parser.parseRSS(completeRSS2Items,new URL("http://www.test.de"))
    }
  }

}
