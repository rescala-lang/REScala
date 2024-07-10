package replication.webapp

import channels.webnativewebsockets.WebsocketConnect
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import org.scalajs.dom
import replication.DataManager

class ContentConnectionManager(dataManager: DataManager[?])(using JsonValueCodec[dataManager.CodecState]) {

  val wsUri: String = {
    val wsProtocol = if dom.document.location.protocol == "https:" then "wss" else "ws"
    val path       = dom.document.location.pathname
    val li         = path.lastIndexOf('/')
    val parent =
      if li < 0
      then "/"
      else path.substring(0, li + 1)
    s"$wsProtocol://${dom.document.location.host}${parent}ws"
  }

  def connect(): Unit = {
    tryConnect()
  }

  def tryConnect(): Unit = {
    println(s"trying to connect to $wsUri")
    dataManager.addLatentConnection(
      WebsocketConnect.connect(wsUri)
    )
  }
}
