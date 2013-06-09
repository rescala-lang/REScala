package reader.network

import java.net.SocketException
import java.net.SocketTimeoutException
import java.net.URL
import java.net.UnknownHostException

import scala.events.Event
import scala.events.Observable
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import scala.xml.NodeSeq
import scala.xml.XML

/**
 * The Fetcher is responsible to fetch the xml data
 * After fetching the data an event is triggered
 */
class Fetcher {
  lazy val rssFetched: Event[(NodeSeq, URL)] = fetch.after map { (_: (URL, NodeSeq)).swap }
  lazy val state =
    ((fetch.before map { _: Any => "Started fetching" }) ||
     (fetch.after map { _: Any => "Finished fetching" })) latest ""
  
  private val urlsToFetch = Var(Set.empty[URL])
  val currentURLs = Signal { urlsToFetch() }
  
  def loadMethod(url: URL) =
    try
      XML.load(url)
    catch {
      case _: UnknownHostException => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException => NodeSeq.Empty
    }
  
  /**
   * Add the given URL to the list of urls to fetch
   */
  def addURL(url: URL) {
    try url.getContent
    catch { case _: UnknownHostException => return }
    
    if (!(urlsToFetch.getValue contains url)) {
      urlsToFetch() += url
      fetch(url) // immediately perform a fetch
    }
  }
  
  /**
   * Removes the url from the list of urls to fetch
   * Does NOT remove the channel from the content!
   */
  val removeURL = Observable { (url: URL) => urlsToFetch() -= url }
  
  /**
   * Fetch the channels from the list of urls
   */
  val fetch = Observable(loadMethod)
  def fetchAll = { urlsToFetch.getValue foreach (fetch(_)) }
}
