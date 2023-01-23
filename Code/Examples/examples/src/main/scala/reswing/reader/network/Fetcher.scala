package reswing.reader.network

import java.net.SocketException
import java.net.SocketTimeoutException
import java.net.URL
import java.net.UnknownHostException

import reswing.reader.Observable

import scala.xml.NodeSeq
import scala.xml.XML
import rescala.default._

/** The Fetcher is responsible to fetch the xml data
  * After fetching the data an event is triggered
  */
class Fetcher(val urls: Signal[Set[URL]]) {
  lazy val rssFetched: Event[(NodeSeq, URL)] = fetch.after map { (_: (URL, NodeSeq)).swap } // #EVT //#EF
  lazy val state: Signal[String] = // #SIG
    ((fetch.before map { (_: Any) => "Started fetching" }) ||          // #EF //#EF
      (fetch.after map { (_: Any) => "Finished fetching" })) latest "" // #EF //#IF

  val firstFetchInitiated = collection.mutable.Set.empty[URL]

  urls.changed += { urls => // #IF //#HDL
    for (url <- urls filterNot (firstFetchInitiated contains _)) {
      firstFetchInitiated += url
      fetch(url)
    }
  }

  def loadMethod(url: URL) =
    try XML.load(url)
    catch {
      case _: UnknownHostException   => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException        => NodeSeq.Empty
    }

  private val fetch = Observable(loadMethod) // #EVT //#EVT

  /** Fetch the channels from the list of urls */

  def fetchAll() = { urls.now foreach (fetch(_)) }
}
