import java.net._
import rescala._
import rescala._
import scala.xml.XML
import scala.xml.NodeSeq

class Fetcher(val url: URL) {

  var listOfUrl: Signal[NodeSeq] = Signal{defaultLoadMethod(url)}
  var ChTitle: Signal[String] = Signal{(listOfUrl()\ "title").text}

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
