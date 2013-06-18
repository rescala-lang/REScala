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
class Fetcher(val urls: Signal[Set[URL]]) {
  lazy val rssFetched: Event[(NodeSeq, URL)] = fetch.after map { (_: (URL, NodeSeq)).swap }
  lazy val state =
    ((fetch.before map { _: Any => "Started fetching" }) ||
     (fetch.after map { _: Any => "Finished fetching" })) latest ""
  
  val firstFetchInitiated = collection.mutable.Set.empty[URL]
  
  urls.changed += { urls =>
    for (url <- urls filterNot (firstFetchInitiated contains _)) {
      firstFetchInitiated += url
      fetch(url)
    }
  }
  
  def loadMethod(url: URL) =
    try
      XML.load(url)
    catch {
      case _: UnknownHostException => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException => NodeSeq.Empty
    }
  
  private val fetch = Observable(loadMethod)
  
  /**
   * Fetch the channels from the list of urls
   */
  
  def fetchAll = { urls.getValue foreach (fetch(_)) }
}
