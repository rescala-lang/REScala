package dtn.routers

import dtn.{DeliveredIncludedBaseRouter, DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class DirectRouter extends DeliveredIncludedBaseRouter {
  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    if (delivered.get(packet.bp.id) == Some(true)) {
      println("bundle was already delivered")
      return None
    }

    val target: Option[DtnPeer] = peers.get(packet.bp.destination.extract_node_endpoint().full_uri)

    var selected_clas: ListBuffer[Sender] = ListBuffer()

    target match
      case None => {
        println("peer not directly known")
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
}
object DirectRouter {
  def create(port: Int): Future[DirectRouter] = {
    val router = DirectRouter()

    WSEroutingClient.create(port).map(ws => {
      router.ws = Option(ws)
      router
    })
  }
}

