package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient, MonitoringClientInterface, NoMonitoringClient}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import java.util.concurrent.ConcurrentHashMap

/*
  spray-and-wait router

  the source holds n copies of the
 */

class SprayAndWaitRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface)
    extends BaseRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface) {

  val sprayDelivered =
    ConcurrentHashMap[String, Set[String]]() // will grow indefinitely as we do not garbage collect here
  val copies = ConcurrentHashMap[String, Int]() // will grow indefinitely as we do not garbage collect here

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val num_copies_before = copies.getOrDefault(packet.bp.id, SprayAndWaitRouter.MAX_COPIES)

    if num_copies_before < 2 || !packet.bp.id.startsWith(ws.nodeId) then {
      println(s"attempting direct routing")

      val target_node_name: String = packet.bp.destination.extract_node_name()

      peers.get(target_node_name) match
        case null => {
          println(s"peer $target_node_name not directly known. not routing.")
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
        }
        case peer: DtnPeer => {
          val selected_clas = peer.cla_list
            .filter((agent, port_option) => packet.clas.contains(agent))
            .map((agent, port_option) =>
              Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid)
            )
            .toList

          println(s"selected clas: ${selected_clas}")
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = true))
        }
    } else {
      println("attempting spray routing")

      val selected_clas = peers.asScala
        .filter((peer_name, peer) => !sprayDelivered.getOrDefault(packet.bp.id, Set()).contains(peer_name))
        .take(num_copies_before - 1) // -1 to be left with at least one copy
        .map((peer_name, peer) => {
          peer.cla_list
            .filter((agent, port_option) => packet.clas.contains(agent))
            .map((agent, port_option) =>
              Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid)
            )
            .take(1) // limiting the number of clas to the number of peers
        })
        .flatten
        .toList

      println(s"selected clas: ${selected_clas}")
      println(s"setting copies from ${num_copies_before} to ${num_copies_before - selected_clas.size}")
      copies.put(packet.bp.id, num_copies_before - selected_clas.size)
      Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = false))
    }
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp}")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}")

    if packet.bid.startsWith(ws.nodeId) then {
      copies.put(packet.bid, copies.get(packet.bid) + 1)
      ()
    }
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}.")

    if packet.bid.startsWith(ws.nodeId) then {
      sprayDelivered.get(packet.bid) match {
        case null =>
          sprayDelivered.put(packet.bid, Set(packet.cla_sender))
          ()
        case x: Set[String] =>
          sprayDelivered.put(packet.bid, (x + packet.cla_sender))
          ()
      }
    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println("received incoming bundle. information not used for routing. ignoring.")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object SprayAndWaitRouter {
  val MAX_COPIES: Int = 7

  def apply(
      host: String,
      port: Int,
      monitoringClient: MonitoringClientInterface = NoMonitoringClient
  ): Future[SprayAndWaitRouter] =
    WSEroutingClient(host, port).map(ws => new SprayAndWaitRouter(ws, monitoringClient))
}
