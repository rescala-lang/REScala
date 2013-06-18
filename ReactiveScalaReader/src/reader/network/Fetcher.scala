package reader.network

import java.net.SocketException
import java.net.SocketTimeoutException
import java.net.URL
import java.net.UnknownHostException

import scala.events.Event
import scala.events.ImperativeEvent
import scala.events.Observable
import scala.xml.NodeSeq
import scala.xml.XML

/**
 * The Fetcher is responsible to fetch the xml data
 * After fetching the data an event is triggered
 */
class Fetcher {
  lazy val rssFetched: Event[(NodeSeq, URL)] = fetch.after map { (_: (URL, NodeSeq)).swap }
  
  lazy val urlAdded = new ImperativeEvent[URL]
  lazy val urlRemoved = new ImperativeEvent[URL]
  
  lazy val startedFetching = fetch.before map { _: Any => "Started fetching" }
  lazy val finishedFetching = fetch.after map { _: Any => "Finished fetching" }
  
  def loadMethod(url: URL) =
    try
      XML.load(url)
    catch {
      case _: UnknownHostException => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException => NodeSeq.Empty
    }
  
  private val fetch = Observable(loadMethod)
  
  private var urlsToFetch = Set.empty[URL]
  def currentURLs = urlsToFetch.toList
  
  /**
   * Add the given URL to the list of urls to fetch
   */
  def addURL(url: URL) {
    try url.getContent
    catch { case _: UnknownHostException => return }

    if (!(urlsToFetch contains url)) {
      urlsToFetch += url
      urlAdded(url)
      fetch(url) // immediately perform a fetch
    }
  }
  
  /**
   * Removes the url from the list of urls to fetch
   * Does NOT remove the channel from the content!
   */
  val removeURL = Observable { (url: URL) =>
    if (!(urlsToFetch contains url)) {
      urlsToFetch -= url
      urlRemoved(url)
    }
  }
  
  /**
   * Fetch the channels from the list of urls
   */
  def fetchAll = { urlsToFetch foreach (fetch(_)) }
}
