package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient}

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

/*
  Includes the standalone EpidemicRouter and the extracted EpidemicStrategy for use in other routers.
 */

class EpidemicRouter(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {

  val epidemicStrategy: EpidemicStrategy = EpidemicStrategy()

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    epidemicStrategy.onRequestSenderForBundle(peers, services, packet)
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
    epidemicStrategy.onSendingSucceeded(packet)
    println(
      s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. added node-name ${packet.cla_sender} to delivered list of bundle."
    )
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println("received incoming bundle. information not used for routing. ignoring.")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object EpidemicRouter {
  def apply(host: String, port: Int): Future[EpidemicRouter] =
    WSEroutingClient(host, port).map(ws => new EpidemicRouter(ws))
}

class EpidemicStrategy {
  val delivered = ConcurrentHashMap[String, Set[String]]() // will grow indefinitely as we do not garbage collect here

  def onRequestSenderForBundle(
      peers: ConcurrentHashMap[String, DtnPeer],
      services: ConcurrentHashMap[Int, String],
      packet: Packet.RequestSenderForBundle
  ): Option[Packet.ResponseSenderForBundle] = {
    val selected_clas = peers.asScala
      .filter((peer_name, peer) => !delivered.getOrDefault(packet.bp.id, Set()).contains(peer_name))
      .map((peer_name, peer) => {
        peer.cla_list
          .filter((agent, port_option) => packet.clas.contains(agent))
          .map((agent, port_option) =>
            Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid)
          )
      })
      .flatten
      .toList

    Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = false))
  }

  def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    delivered.get(packet.bid) match {
      case null =>
        delivered.put(packet.bid, Set(packet.cla_sender))
        ()
      case x: Set[String] =>
        delivered.put(packet.bid, (x + packet.cla_sender))
        ()
    }
  }
}
