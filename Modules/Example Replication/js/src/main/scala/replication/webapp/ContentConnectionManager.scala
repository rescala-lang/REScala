package replication.webapp

import channel.BiChan
import channel.webnativewebsockets.WebsocketConnect
import loci.transmitter.RemoteRef
import org.scalajs.dom
import reactives.default.*
import replication.DataManager

import scala.util.{Failure, Success}

class ContentConnectionManager(dataManager: DataManager[?]) {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    val path       = dom.document.location.pathname
    val li         = path.lastIndexOf('/')
    val parent =
      if li < 0
      then "/"
      else path.substring(0, li + 1)
    s"$wsProtocol://${dom.document.location.host}${parent}ws"
  }

  val connectedRemotes = Fold(Map.empty[RemoteRef, Boolean])()

  def connect(): Unit = {
    tryConnect()
  }

  def tryConnect(): Unit = {
    println(s"trying to connect to $wsUri")
    WebsocketConnect.connect(wsUri).run(using ()):
      case Success(conn) => dataManager.addConnection(BiChan(conn, conn))
      case Failure(t)    => t.printStackTrace()
  }
}
