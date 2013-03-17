package reader.network

import reader.testHelpers._
import reader.XMLFixtures._
import reader.network._

import org.scalatest._
import org.scalatest.matchers._

import scala.xml.NodeSeq

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.net.URL
import scala.events.ImperativeEvent

@RunWith(classOf[JUnitRunner])
class FetcherSpec extends FlatSpec with ShouldMatchers with BeforeAndAfter {
  var fetcher: Fetcher = _

  var validFeed = new URL("http://feeds.rssboard.org/rssboard")
  val invalidFeed = new URL("http://www.test.de")

  before {
    fetcher = new Fetcher
  }

  "The Fetcher" should "fetch and fire after adding an url" in {
    fetcher.loadMethod = { _ => simpleRSS }
    shouldFire(fetcher.rssFetched) {
      fetcher.addURL(validFeed)
    }
  }

  it should "not add unreachable urls" in {
    fetcher.loadMethod = { _ => NodeSeq.Empty }
    var numUrls = fetcher.currentURLs.size
    fetcher.addURL(new URL("http://te.de"))
    fetcher.currentURLs should have size numUrls
  }

  it should "add urls only once" in {
    fetcher.loadMethod = { _ => NodeSeq.Empty }
    val numURLs = fetcher.currentURLs.length
    fetcher.addURL(validFeed)
    fetcher.addURL(validFeed)
    fetcher.currentURLs should have length (numURLs+1)
  }

  it should "fire an event once after adding a new url" in {
    fetcher.loadMethod = { _ => NodeSeq.Empty }
    shouldFire(fetcher.urlAdded) { fetcher.addURL(validFeed) }
    shouldNotFire(fetcher.urlAdded) { fetcher.addURL(validFeed) }
  }

}
