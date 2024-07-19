package dtn.routing

import dtn.{DtnPeer, Endpoint, Packet, PreviousNodeBlock, RdtMetaBlock, Sender, WSEroutingClient}
import rdts.time.Dots

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.math.{addExact, max}
import scala.util.{Random, Try}

/*
  This alternative RdtRouter does a simple limited flooding approach.

  Essentially it gathers all dot-state information it can and
  forwards bundles to those who it things do not have that information.

  To be able to merge source-node-information, the bundle-source will be a unicast endpoint like: dtn://node-id/rdt/app-name
  In contrast to the bundle-destination, which will be a group endpoint like: dtn://global/~rdt/app-name

  Information Gathering:
    1. On an rdt-bundle-reception, merge the dots for the source-node and previous-node
    2. On a succesfull bundle-forwarding, merge the dots for the forwarded-node
  --> Note: all these nodes should now also have that state (and should also have seen that bundle)
  --> Note: this dot-store is essentially the same as in the RdtRouter.scala

  Forwarding:
    1. If a bundle-request was sent without any meta-information, we cannot route this bundle, ignore request.
    3. Filter all peers for those who do not know this dot-state.
    4. Map peers to clas and return the sender-list.
 */

class RdtRouter2(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {
  // state will grow indefinitely. there is currently no garbage collector
  val dotState: DotState2 = DotState2()

  // grows indefinitely
  val tempDotsStore: ConcurrentHashMap[String, Dots]    = ConcurrentHashMap()
  val tempRdtIdStore: ConcurrentHashMap[String, String] = ConcurrentHashMap()

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val rdt_id: String = packet.bp.destination.extract_endpoint_id()

    if !tempDotsStore.contains(packet.bp.id) then {
      println(s"bundle meta information for bundle-id ${packet.bp.id} are not available. not routing rn.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    val dots: Dots = tempDotsStore.getOrDefault(packet.bp.id, Dots.empty)

    val all_peers = peers.values().asScala
    println(s"all peers: ${all_peers.toList}")

    // filter peers for peers that still need this bundles dot-state
    val filtered_peers = dotState.filterPeers(all_peers, rdt_id, dots)
    println(s"filtered peers: ${filtered_peers.toList}")

    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    val selected_clas: Iterable[Sender] = filtered_peers.flatMap(target => {
      target.cla_list
        .filter((agent, port_option) => packet.clas.contains(agent))
        .map((agent, port_option) =>
          Sender(remote = target.addr, port = port_option, agent = agent, next_hop = target.eid)
        )
    })
    println(s"selected clas: ${selected_clas.toList}")

    return Option(Packet.ResponseSenderForBundle(
      bp = packet.bp,
      clas = selected_clas.toList,
      delete_afterwards = false
    ))
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp.id}.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    val dots   = tempDotsStore.get(packet.bid)
    val rdt_id = tempRdtIdStore.get(packet.bid)

    if dots != null && rdt_id != null then {
      println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. merging dots.")
      dotState.mergeDots(Endpoint.createFromName(packet.cla_sender), rdt_id, dots)
    } else {
      println(
        s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. could not merge dots because dots or rdt_id are no longer available in temp-store."
      )
    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    // we always merge the bundle information, as we might receive it from multiple nodes, which is valuable information to us

    val source_node                     = packet.bndl.primary_block.source.extract_node_endpoint()
    val rdt_id                          = packet.bndl.primary_block.destination.extract_endpoint_id()
    var previous_node: Option[Endpoint] = None
    var dots: Option[Dots]              = None

    packet.bndl.other_blocks.collectFirst {
      case x: PreviousNodeBlock => x
    } match {
      case None                      => println("received incoming bundle without previous node block. ignoring")
      case Some(previous_node_block) => previous_node = Option(previous_node_block.previous_node_id)
    }
    packet.bndl.other_blocks.collectFirst {
      case x: RdtMetaBlock => x
    } match {
      case None                 => println("received incoming bundle without rdt-meta block. ignoring")
      case Some(rdt_meta_block) => dots = Option(rdt_meta_block.dots)
    }

    if previous_node.nonEmpty && dots.nonEmpty then {
      println(s"received incoming bundle ${packet.bndl.id} with rdt-meta block and previous-node block. merging.")
      dotState.mergeDots(source_node, rdt_id, dots.get)
      dotState.mergeDots(previous_node.get, rdt_id, dots.get)

      tempDotsStore.put(packet.bndl.id, dots.get)
      tempRdtIdStore.put(packet.bndl.id, rdt_id)
      ()
    }
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    // todo: if bundle on IncomingBundle is stored temporarily, then use that info to increase the score
    // currently irrelevant, as this message is never sent by the dtnd
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object RdtRouter2 {
  def apply(host: String, port: Int): Future[RdtRouter] = WSEroutingClient(host, port).map(ws => new RdtRouter(ws))
}

class DotState2 {
  // structure: map[rdt-group-id, map[node-id, Dots]]
  val map: ConcurrentHashMap[String, ConcurrentHashMap[Endpoint, Dots]] = ConcurrentHashMap()

  /*
    adds/merges this nodes' dots to the data structure.
   */
  def mergeDots(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Unit = {
    map.putIfAbsent(rdt_id, ConcurrentHashMap())
    map.get(rdt_id).merge(node_endpoint, dots, (x1, x2) => x1.merge(x2))
    ()
  }

  /*
    return all peers for which the provided dots are not already known to them
   */
  def filterPeers(peers: Iterable[DtnPeer], rdt_id: String, dots: Dots): Iterable[DtnPeer] = {
    peers.filter(peer => {
      val d = map.get(rdt_id) match
        case null                                        => Dots.empty
        case node_map: ConcurrentHashMap[Endpoint, Dots] => node_map.getOrDefault(peer.eid, Dots.empty)

      !(dots <= d) || dots.isEmpty
    })
  }
}
