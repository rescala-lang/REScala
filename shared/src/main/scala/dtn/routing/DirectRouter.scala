package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class DirectRouter(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {
  var delivered: Set[String] = Set()  // will grow indefinitely as we do not garbage collect here

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    if (delivered.contains(packet.bp.id)) {
      println("bundle was already delivered. deleting.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = true))
    }

    val target_node_name: String = packet.bp.destination.extract_node_name()

    peers.get(target_node_name) match
      case None => {
        println(s"peer $target_node_name not directly known. not routing.")
        Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
      }
      case Some(peer) => {
        val selected_clas = peer.cla_list
          .filter((agent, port_option) => packet.clas.contains(agent))
          .map((agent, port_option) => Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid))
          .toList

        println(s"selected clas: ${selected_clas}")
        Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = true))
      }
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp}.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}.")
    delivered += packet.bid
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println("received incoming bundle. information not used for routing. ignoring.")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object DirectRouter {
  def apply(host: String, port: Int): Future[DirectRouter] = WSEroutingClient(host, port).map(ws => new DirectRouter(ws))
}

