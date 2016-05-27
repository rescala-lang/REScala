import java.net._
import scala.events._
import scala.events.behaviour._
import scala.xml.XML
import scala.xml.NodeSeq

class Fetcher(val url: URL) {

  var listOfUrl: Signal[NodeSeq] = Signal{defaultLoadMethod(url)}
  var ChTitle: Signal[String] = Signal{(listOfUrl.getValue\ "title").text}
  
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