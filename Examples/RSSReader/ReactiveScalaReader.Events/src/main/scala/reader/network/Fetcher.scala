package reader.network

import java.net.SocketException
import java.net.SocketTimeoutException
import java.net.URL
import java.net.UnknownHostException

import reader.Observable
import rescala._

import scala.xml.NodeSeq
import scala.xml.XML

/**
 * The Fetcher is responsible to fetch the xml data
 * After fetching the data an event is triggered
 */
class Fetcher {
  lazy val rssFetched: Event[(NodeSeq, URL)] = fetch.after map { (_: (URL, NodeSeq)).swap } //#EVT //#EF

  lazy val urlAdded = Evt[URL] //#EVT
  lazy val urlRemoved = Evt[URL] //#EVT

  lazy val startedFetching = fetch.before map { _: Any => "Started fetching" } //#EVT //#EF
  lazy val finishedFetching = fetch.after map { _: Any => "Finished fetching" } //#EVT //#EF

  def loadMethod(url: URL) =
    try
      XML.load(url)
    catch {
      case _: UnknownHostException => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException => NodeSeq.Empty
    }

  private val fetch = Observable(loadMethod)  //#EVT //#EVT

  private var urlsToFetch = Set.empty[URL]
  def currentURLs = urlsToFetch.toList

  /**
   * Add the given URL to the list of urls to fetch
   */
  def addURL(url: URL): Unit = {
    try url.getContent
    catch { case _: UnknownHostException => return }

    if (!(urlsToFetch contains url)) {
      urlsToFetch += url
      urlAdded.fire(url)
      fetch(url) // immediately perform a fetch
    }
  }

  /**
   * Removes the url from the list of urls to fetch
   * Does NOT remove the channel from the content!
   */
  val removeURL = Observable { (url: URL) =>  //never used
    if (!(urlsToFetch contains url)) {
      urlsToFetch -= url
      urlRemoved.fire(url)
    }
  }

  /**
   * Fetch the channels from the list of urls
   */
  def fetchAll() = { urlsToFetch foreach (fetch(_)) }
}
