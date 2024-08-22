package dtn.routing

import dtn.{DtnPeer, Packet, Sender, WSEroutingClient, MonitoringClientInterface, NoMonitoringClient}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import java.util.concurrent.ConcurrentHashMap
import java.time.ZonedDateTime
import dtn.Endpoint
import scala.util.Random
import dtn.PreviousNodeBlock

/*
  The RandomSprayRouter implements the backup strategy only of the RdtRouter
 */

class RandomSprayRouter(
    ws: WSEroutingClient,
    monitoringClient: MonitoringClientInterface,
    nTotalNodes: Int,
    topNNeighbours: Int
) extends BaseRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface) {

  // will grow indefinitely as we do not garbage collect here
  val tempPreviousNodeStore: ConcurrentHashMap[String, Endpoint] = ConcurrentHashMap()
  val delivered                                                  = ConcurrentHashMap[String, Set[String]]()

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    if delivered.getOrDefault(packet.bp.id, Set()).size >= nTotalNodes then {
      println("bundle was forwarded to enough unique neighbours. deleting.")
      tempPreviousNodeStore.remove(packet.bp.id)
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = true))
    }

    val source_node: Endpoint = packet.bp.source.extract_node_endpoint()

    var random_peers: Iterable[DtnPeer] = peers.values().asScala

    // remove previous node and source node from peers if available
    random_peers = tempPreviousNodeStore.get(packet.bp.id) match
      case null => random_peers.filter(p => !p.eid.equals(source_node))
      case previous_node: Endpoint =>
        random_peers.filter(p => !p.eid.equals(previous_node) && !p.eid.equals(source_node))

    println(s"filtered random peers available: ${List.from(random_peers).map(peer => peer.eid)}")

    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    val selected_clas: List[Sender] = Random.shuffle(random_peers).take(topNNeighbours).flatMap(target => {
      target.cla_list
        .filter((agent, port_option) => packet.clas.contains(agent))
        .map((agent, port_option) =>
          Sender(remote = target.addr, port = port_option, agent = agent, next_hop = target.eid)
        )
    }).toList
    println(s"selected clas: $selected_clas")

    println(s"time: ${ZonedDateTime.now()}")

    return Option(Packet.ResponseSenderForBundle(
      bp = packet.bp,
      clas = selected_clas,
      delete_afterwards = false
    ))
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
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}.")
    delivered.get(packet.bid) match {
      case null =>
        delivered.put(packet.bid, Set(packet.cla_sender))
        ()
      case x: Set[String] =>
        delivered.put(packet.bid, (x + packet.cla_sender))
        ()
    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    println("received incoming bundle. extracting previous node info")
    packet.bndl.other_blocks.collectFirst {
      case x: PreviousNodeBlock => x
    } match {
      case None                      => println("received incoming bundle without previous node block. ignoring")
      case Some(previous_node_block) => tempPreviousNodeStore.put(packet.bndl.id, previous_node_block.previous_node_id)
    }
    ()
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object RandomSprayRouter {
  val N_TOTAL_NODES    = 10
  val TOP_N_NEIGHBOURS = 3

  def apply(
      host: String,
      port: Int,
      monitoringClient: MonitoringClientInterface = NoMonitoringClient,
      nTotalNodes: Int = N_TOTAL_NODES,
      topNNeighbours: Int = TOP_N_NEIGHBOURS,
  ): Future[RandomSprayRouter] =
    WSEroutingClient(host, port).map(ws => new RandomSprayRouter(ws, monitoringClient, nTotalNodes, topNNeighbours))
}
