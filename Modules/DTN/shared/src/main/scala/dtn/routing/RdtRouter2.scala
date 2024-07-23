package dtn.routing

import dtn.{DtnPeer, Endpoint, Packet, PreviousNodeBlock, RdtMetaBlock, Sender, WSEroutingClient, RdtMetaInfo, RdtMessageType}
import rdts.time.Dots

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.math.{addExact, max}
import scala.util.{Random, Try}

/*
  This alternative RdtRouter does a simple limited flooding approach.
  It also only routes rdt-bundles. Other bundles are currently ignored.

  There are two types of rdt-bundles:
    1. Request-Bundles, which only contains a dot-set that represents the known state and thereby requests anything unknown.
    2. Payload-Bundles, which contain a dot-set and a payload, where the dot-set represents everything included in the payload.

  Request-Bundle routing:
    We use epidemic routing here, as the dot-set without a payload does not contribute to our known state that we can intelligently route.
    Also, the requests should reach everybody as fast as possible with as few messages as possible, because we do not know who is contributing to our rdt.

  Payload-Bundle routing:
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
  // epidemic routing is for rdt-request-bundles. they do not contain any payload that contributes to our state (in contrast to rdt-payload-bundles)
  val epidemicStrategy: EpidemicStrategy = EpidemicStrategy()

  // state will grow indefinitely. there is currently no garbage collector
  val dotState: DotState2 = DotState2()

  // grows indefinitely
  val tempRdtMetaInfoStore: ConcurrentHashMap[String, RdtMetaInfo] = ConcurrentHashMap()
  val tempRdtIdStore: ConcurrentHashMap[String, String]            = ConcurrentHashMap()

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    // we only route rdt packets rn
    if !tempRdtMetaInfoStore.contains(packet.bp.id) then {
      println(s"no bundle meta information available for bundle-id ${packet.bp.id}. not routing.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    val rdt_id: String             = packet.bp.destination.extract_endpoint_id()
    val rdt_meta_info: RdtMetaInfo = tempRdtMetaInfoStore.get(packet.bp.id)

    rdt_meta_info.message_type match
      case RdtMessageType.Request => {
        println("got rdt request bundle. routing with epidemic strategy")
        return epidemicStrategy.onRequestSenderForBundle(peers, services, packet)
      }
      case RdtMessageType.Payload => {
        println("got rdt payload bundle. routing with rdt strategy")
      }

    val all_peers = peers.values().asScala
    println(s"all peers: ${all_peers.toList}")

    // filter peers for peers that still need this bundles dot-state
    val filtered_peers = dotState.filterPeers(all_peers, rdt_id, rdt_meta_info.dots)
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
    val rdt_id        = tempRdtIdStore.get(packet.bid)
    val rdt_meta_info = tempRdtMetaInfoStore.get(packet.bid)

    print(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. ")

    if rdt_meta_info == null || rdt_id == null then {
      println("no rdt-meta-information are available. ignoring")
    } else {
      rdt_meta_info.message_type match
        case RdtMessageType.Request => {
          println("rdt-request-message. feeding epidemic strat")
          epidemicStrategy.onSendingSucceeded(packet)
        }
        case RdtMessageType.Payload => {
          println("rdt-payload-message. merging dots for next hop")
          dotState.mergeDots(Endpoint.createFromName(packet.cla_sender), rdt_id, rdt_meta_info.dots)
        }

    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    // we always merge the bundle information, as we might receive it from multiple nodes, which is valuable information to us

    val source_node                        = packet.bndl.primary_block.source.extract_node_endpoint()
    val rdt_id                             = packet.bndl.primary_block.destination.extract_endpoint_id()
    var previous_node: Option[Endpoint]    = None
    var rdt_meta_info: Option[RdtMetaInfo] = None

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
      case Some(rdt_meta_block) => rdt_meta_info = Option(rdt_meta_block.info)
    }

    if previous_node.nonEmpty && rdt_meta_info.nonEmpty then {
      print(s"received incoming bundle ${packet.bndl.id} with rdt-meta block and previous-node block. ")

      rdt_meta_info.get.message_type match
        case RdtMessageType.Request => {
          println("rdt-request-message. merging only source node")
          dotState.mergeDots(source_node, rdt_id, rdt_meta_info.get.dots)
        }
        case RdtMessageType.Payload => {
          println("rdt-payload-message. merging source and previous node")
          dotState.mergeDots(source_node, rdt_id, rdt_meta_info.get.dots)
          dotState.mergeDots(previous_node.get, rdt_id, rdt_meta_info.get.dots)
        }

      tempRdtMetaInfoStore.put(packet.bndl.id, rdt_meta_info.get)
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
