package dtn.routing

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import dtn.{DtnPeer, Packet, WSEroutingClient}


trait Routing {
  def peers: Map[String, DtnPeer]
  def services: Map[Int, String]

  def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle]
  def onError(packet: Packet.Error): Unit
  def onTimeout(packet: Packet.Timeout): Unit
  def onSendingFailed(packet: Packet.SendingFailed): Unit
  def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit
  def onIncomingBundle(packet: Packet.IncomingBundle): Unit
  def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit
  def onEncounteredPeer(packet: Packet.EncounteredPeer): Unit
  def onDroppedPeer(packet: Packet.DroppedPeer): Unit
  def onPeerState(packet: Packet.PeerState): Unit
  def onServiceState(packet: Packet.ServiceState): Unit
}

abstract class BaseRouter(ws: WSEroutingClient) extends Routing {
  var peers: Map[String, DtnPeer] = Map()
  var services: Map[Int, String] = Map()

  def start_receiving(): Future[Unit] = {
    ws.receivePacket().flatMap(packet => {
      on_packet_received(packet)
      start_receiving()
    })
  }

  def on_packet_received(packet: Packet): Unit = {
    packet match
      case p: Packet.RequestSenderForBundle => {
        onRequestSenderForBundle(p) match {
          case None => {}  // nothing to send
          case Some(response) => ws.sendPacket(response)
        }
      }
      case p: Packet.Error => onError(p)
      case p: Packet.Timeout => onTimeout(p)
      case p: Packet.SendingFailed => onSendingFailed(p)
      case p: Packet.SendingSucceeded => onSendingSucceeded(p)
      case p: Packet.IncomingBundle => onIncomingBundle(p)
      case p: Packet.IncomingBundleWithoutPreviousNode => onIncomingBundleWithoutPreviousNode(p)
      case p: Packet.EncounteredPeer => onEncounteredPeer(p)
      case p: Packet.DroppedPeer => onDroppedPeer(p)
      case p: Packet.PeerState => onPeerState(p)
      case p: Packet.ServiceState => onServiceState(p)
      case p: Packet => println(s"warning: received unkown/unexpected packet $p for erouter. ignoring.")
  }
  
  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = ???
  override def onError(packet: Packet.Error): Unit = ???
  override def onTimeout(packet: Packet.Timeout): Unit = ???
  override def onSendingFailed(packet: Packet.SendingFailed): Unit = ???
  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = ???
  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = ???
  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = ???
  override def onEncounteredPeer(packet: Packet.EncounteredPeer): Unit = {
    if (!peers.contains(packet.name)) {
      println(s"encountered new peer: ${packet.name}")  // limit log file spam
    }
    peers += (packet.name -> packet.peer)
  }
  override def onDroppedPeer(packet: Packet.DroppedPeer): Unit = {
    println(s"dropped peer: ${packet.name}")
    peers -= packet.name
  }
  override def onPeerState(packet: Packet.PeerState): Unit = {
    println(s"received initial peer list: ${packet.peers}")
    peers = packet.peers
  }
  override def onServiceState(packet: Packet.ServiceState): Unit = {
    println(s"received initial service list: ${packet.service_list}")
    services = packet.service_list
  }
}
