package dtn.routers

import dtn.{BaseRouter, DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class DirectRouter extends BaseRouter {
  var delivered: Map[String, Boolean] = Map()  // will grow indefinitely as we do not garbage collect here

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    if (delivered.get(packet.bp.id) == Some(true)) {
      println("bundle was already delivered")
      return None
    }

    val target_node_name: String = packet.bp.destination.extract_node_name()
    val target: Option[DtnPeer] = peers.get(target_node_name)

    var selected_clas: ListBuffer[Sender] = ListBuffer()

    target match
      case None => {
        println(s"peer $target_node_name not directly known")
        None
      }
      case Some(peer) => {
        for (cla <- peer.cla_list) {
          if (packet.clas.contains(cla(0))) {
            selected_clas += Sender(remote = peer.addr, port = cla(1), agent = cla(0), next_hop = peer.eid)
          }
        }
        Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas.toList, delete_afterwards = true))
      }
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp}. setting delivered[${packet.bp.id}]=false")
    delivered += (packet.bp.id -> false)
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}. setting delivered[${packet.bid}]=false")
    delivered += (packet.bid -> false)
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. setting delivered[${packet.bid}]=true")
    delivered += (packet.bid -> true)
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println(s"received incoming bundle. information not used for routing. ignoring. message: ${packet}")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println(s"received incoming bundle without previous node. information not used for routing. ignoring. message: $packet")
  }
}
object DirectRouter {
  def apply(port: Int): Future[DirectRouter] = {
    val router = new DirectRouter()

    WSEroutingClient.create(port).map(ws => {
      router.ws = Option(ws)
      router
    })
  }
}

