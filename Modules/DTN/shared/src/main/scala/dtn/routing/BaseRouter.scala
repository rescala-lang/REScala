package dtn.routing

import dtn.{MonitoringMessage, DtnPeer, Packet, WSEroutingClient, printError}

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import dtn.MonitoringClientInterface

trait Routing {
  def peers: ConcurrentHashMap[String, DtnPeer]
  def services: ConcurrentHashMap[Int, String]

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

abstract class BaseRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface) extends Routing {
  val peers: ConcurrentHashMap[String, DtnPeer] = ConcurrentHashMap()
  val services: ConcurrentHashMap[Int, String]  = ConcurrentHashMap()

  def start_receiving(): Future[Unit] = {
    ws.receivePacket().flatMap(packet => {
      on_packet_received(packet).flatMap(_ => {
        start_receiving()
      })
    }).recover(_.printStackTrace())
  }

  def on_packet_received(packet: Packet): Future[Unit] = {
    packet match
      case p: Packet.RequestSenderForBundle => {
        onRequestSenderForBundle(p) match {
          case None           => Future({}) // nothing to send
          case Some(response) => ws.sendPacket(response)
        }
      }
      case p: Packet.Error         => Future(onError(p))
      case p: Packet.Timeout       => Future(onTimeout(p))
      case p: Packet.SendingFailed => Future(onSendingFailed(p))
      case p: Packet.SendingSucceeded => {
        monitoringClient.send(MonitoringMessage.BundleForwardedAtRouter(ws.nodeId, p.bid))
        Future(onSendingSucceeded(p))
      }
      case p: Packet.IncomingBundle => {
        monitoringClient.send(MonitoringMessage.BundleReceivedAtRouter(ws.nodeId, p.bndl.id))
        Future(onIncomingBundle(p))
      }
      case p: Packet.IncomingBundleWithoutPreviousNode => Future(onIncomingBundleWithoutPreviousNode(p))
      case p: Packet.EncounteredPeer                   => Future(onEncounteredPeer(p))
      case p: Packet.DroppedPeer                       => Future(onDroppedPeer(p))
      case p: Packet.PeerState                         => Future(onPeerState(p))
      case p: Packet.ServiceState                      => Future(onServiceState(p))
      case p: Packet => Future(println(s"warning: received unkown/unexpected packet $p for erouter. ignoring."))
  }

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] =
    ???
  override def onError(packet: Packet.Error): Unit                                                         = ???
  override def onTimeout(packet: Packet.Timeout): Unit                                                     = ???
  override def onSendingFailed(packet: Packet.SendingFailed): Unit                                         = ???
  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit                                   = ???
  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit                                       = ???
  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = ???
  override def onEncounteredPeer(packet: Packet.EncounteredPeer): Unit = {
    if !peers.containsKey(packet.name) then {
      println(s"encountered new peer: ${packet.name}") // limit log file spam
    }
    peers.put(packet.name, packet.peer)
    ()
  }
  override def onDroppedPeer(packet: Packet.DroppedPeer): Unit = {
    println(s"dropped peer: ${packet.name}")
    peers.remove(packet.name)
    ()
  }
  override def onPeerState(packet: Packet.PeerState): Unit = {
    println(s"received initial peer list: ${packet.peers}")
    peers.clear()
    peers.putAll(packet.peers.asJava)
  }
  override def onServiceState(packet: Packet.ServiceState): Unit = {
    println(s"received initial service list: ${packet.service_list}")
    services.clear()
    services.putAll(packet.service_list.asJava)
  }
}
