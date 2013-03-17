package reader.network

import scala.events._
import scala.events.EventsLibConversions._
import reader.common._
import scala.collection.mutable.HashMap
import scala.xml.XML

import java.net._

import scala.xml.Elem
import scala.xml.NodeSeq

/**
 * The Fetcher is responsible to fetch the xml data
 * After fetching the data an event is triggered
 */
class Fetcher {

  val trigger = new ImperativeEvent[Unit]
  trigger += { _ => fetch() }

  lazy val rssFetched = fetch1.after map { x: (URL,NodeSeq) => x.swap }

  lazy val urlAdded = new ImperativeEvent[URL]
  lazy val urlRemoved: Event[URL] = removeURL.after map { arg_result: (URL,Unit) => arg_result._1 }

  lazy val startedFetching = fetch.before.map { _: Any => "Started fetching" }
  lazy val finishedFetching = fetch.after.map { _: Any => "Finished fetching" }

  val defaultLoadMethod = (url: URL) => {
    try {
      XML.load(url)
    } catch {
      case _: UnknownHostException => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException => NodeSeq.Empty
    }

  }

  var loadMethod = defaultLoadMethod

  private def loadChannel(url: URL) = loadMethod(url) \ "channel"

  private var urlsToFetch: Set[URL] = Set()
  def currentURLs: List[URL] = urlsToFetch.toList

  /**
   * Add the given URL to the list of urls to fetch
   */
  def addURL(url: URL) {
    try { url.getContent }
    catch { case _: UnknownHostException => return }

    if (!urlsToFetch.contains(url)) {
      urlAdded(url)

      // Immediately perform a fetch to reduce the time between adding an url in
      // the gui and seeing the channel appear in the list
      fetch1(url)
    }

    urlsToFetch += url
  }

  /**
   * Removes the url from the list of urls to fetch
   * Does NOT remove the channel from the content!
   */
  val removeURL = Observable { (u: URL) => _removeURL(u) }
  def _removeURL(url: URL) { urlsToFetch -= url }

  /**
   * Fetch the channels from the list of urls
   */
  val fetch = Observable(() => fetchSilent)

  private def fetchSilent { urlsToFetch.foreach { fetch1(_) } }
  private val fetch1 = Observable { (url: URL) => loadMethod(url) }
}
