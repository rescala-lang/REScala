package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class EpidemicRouter(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {
  var delivered: Map[String, Set[String]] = Map()  // will grow indefinitely as we do not garbage collect here

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val selected_clas = peers
      .filter((peer_name, peer) => !delivered.getOrElse(packet.bp.id, Set()).contains(peer_name))
      .map((peer_name, peer) => {
        peer.cla_list
          .filter((agent, port_option) => packet.clas.contains(agent))
          .map((agent, port_option) => Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid))
      })
      .flatten
      .toList
    
    Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = false))
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp}")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    delivered.get(packet.bid) match {
      case None => delivered += (packet.bid -> Set(packet.cla_sender))
      case Some(set) => delivered += (packet.bid -> (set + packet.cla_sender))
    }
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. added node-name ${packet.cla_sender} to delivered list of bundle ${packet.bid} -> ${delivered.get(packet.bid)}")
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println("received incoming bundle. information not used for routing. ignoring.")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object EpidemicRouter {
  def apply(host: String, port: Int): Future[EpidemicRouter] = WSEroutingClient(host, port).map(ws => new EpidemicRouter(ws))
}