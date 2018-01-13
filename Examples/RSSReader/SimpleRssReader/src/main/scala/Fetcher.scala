import java.net._

import rescala._

import scala.xml.{NodeSeq, XML}

class Fetcher(val url: URL) {

  val listOfUrl: Signal[NodeSeq] = Signal{defaultLoadMethod(url)}
  val ChTitle: Signal[String] = Signal{(listOfUrl()\ "title").text}

  private def defaultLoadMethod(urlA: URL): NodeSeq = {
    var channel: NodeSeq = null
    try {
      channel = XML.load(urlA) \ "channel"
    } catch {
      case _: UnknownHostException => NodeSeq.Empty
      case _: SocketTimeoutException => NodeSeq.Empty
      case _: SocketException => NodeSeq.Empty
    }
    return channel
  }




}
