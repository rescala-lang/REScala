package reader.network

import java.net.URL

import scala.xml.NodeSeq

import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import reader.EventShouldFireWrapper.convertToEventShouldFireWrapper
import reader.XMLFixtures.simpleRSS

@RunWith(classOf[JUnitRunner])
class FetcherSpec extends FlatSpec with ShouldMatchers with BeforeAndAfter {
  val validFeed = new URL("http://feeds.rssboard.org/rssboard")
  val invalidFeed = new URL("http://www.test.de")
  
  "The Fetcher" should "fetch and fire after adding an url" in {
    val fetcher = new Fetcher { override def loadMethod(url: URL) = simpleRSS }
    fetcher.rssFetched shouldFireIn {
      fetcher.addURL(validFeed)
    }
  }
  
  it should "not add unreachable urls" in {
    val fetcher = new Fetcher { override def loadMethod(url: URL) = NodeSeq.Empty }
    var numUrls = fetcher.currentURLs.size
    fetcher.addURL(new URL("http://te.de"))
    fetcher.currentURLs should have size numUrls
  }
  
  it should "add urls only once" in {
    val fetcher = new Fetcher { override def loadMethod(url: URL) = NodeSeq.Empty }
    val numURLs = fetcher.currentURLs.length
    fetcher.addURL(validFeed)
    fetcher.addURL(validFeed)
    fetcher.currentURLs should have length (numURLs+1)
  }
  
  it should "fire an event once after adding a new url" in {
    val fetcher = new Fetcher { override def loadMethod(url: URL) = NodeSeq.Empty }
    fetcher.urlAdded shouldFireIn { fetcher.addURL(validFeed) }
    fetcher.urlAdded shouldNotFireIn { fetcher.addURL(validFeed) }
  }
}
