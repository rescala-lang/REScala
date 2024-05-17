package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class EpidemicRouter extends BaseRouter {
  var delivered: Map[String, Set[String]] = Map()  // will grow indefinitely as we do not garbage collect here

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    var selected_clas: ListBuffer[Sender] = ListBuffer()

    for ((peer_name, peer) <- peers) {
      if (!delivered.contains(packet.bp.id) || !delivered.get(peer_name).get.contains(peer_name)) {
        for (cla <- peer.cla_list) {
          if (packet.clas.contains(cla(0))) {
            selected_clas += Sender(remote = peer.addr, port = cla(1), agent = cla(0), next_hop = peer.eid)
          }
        }
      }
    }
    Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas.toList, delete_afterwards = true))
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
    println(s"received incoming bundle. information not used for routing. ignoring. message: ${packet}")
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println(s"received incoming bundle without previous node. information not used for routing. ignoring. message: $packet")
  }
}
object EpidemicRouter {
  def apply(host: String, port: Int): Future[EpidemicRouter] = {
    val router = new EpidemicRouter()

    WSEroutingClient(host, port).map(ws => {
      router.ws = Option(ws)
      router
    })
  }
}