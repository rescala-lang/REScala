package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient, MonitoringClientInterface, NoMonitoringClient}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

/*
  Simple flooding router. Selects every cla available, always.
 */

class FloodingRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface)
    extends BaseRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface) {

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val selected_clas = peers.asScala
      .map((peer_name, peer) => {
        peer.cla_list
          .filter((agent, port_option) => packet.clas.contains(agent))
          .map((agent, port_option) =>
            Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid)
          )
      })
      .flatten
      .toList

    println(s"selected clas: ${selected_clas}")
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
    println(
      s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}."
    )
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println("received incoming bundle. information not used for routing. ignoring.")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object FloodingRouter {
  def apply(
      host: String,
      port: Int,
      monitoringClient: MonitoringClientInterface = NoMonitoringClient
  ): Future[FloodingRouter] =
    WSEroutingClient(host, port).map(ws => new FloodingRouter(ws, monitoringClient))
}
