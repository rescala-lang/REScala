package dtn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


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

abstract class BaseRouter extends Routing {
  var peers: Map[String, DtnPeer] = Map()
  var services: Map[Int, String] = Map()

  var ws: Option[WSEroutingClient] = None

  def start_receiving(): Future[Unit] = {
    ws.get.receivePacket().flatMap(packet => {
      on_packet_received(packet)
      start_receiving()
    })
  }

  def on_packet_received(packet: Packet): Unit = {
    packet match
      case p: Packet.RequestSenderForBundle => {
        ws match {
          case None => println("warning: received RequestSenderForBundle but no websocket is connected. this should never be the case. cannot send response.")
          case Some(websocket) => onRequestSenderForBundle(p) match {
            case None => {}  // nothing to send
            case Some(response) => websocket.sendPacket(response)
          }
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
    println(s"encountered new peer: ${packet.peer}")
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

abstract class DeliveredIncludedBaseRouter extends BaseRouter {
  var delivered: Map[String, Boolean] = Map()  // will grow indefinitely as we do not garbage collect here

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
