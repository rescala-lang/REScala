package dtn.routing

import dtn.{Bundle, DtnPeer, Endpoint, Packet, PreviousNodeBlock, RdtMetaBlock, Sender, WSEroutingClient}
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}
import scala.jdk.CollectionConverters._
import kofre.time.Dots
import math.{max, addExact}
import java.util.concurrent.ConcurrentHashMap


/*
  We need to determine two things in this router for a given bundle:

  1. to which destinations must we forward this bundle?

      what do we do:

        the endpoint naming scheme is: dtn://global/~rdt/app-name for the destination.
        to be able to gather routing information the source will be like: dtn://node-id/rdt/app-name
        
        this means that each rdt-app must subscribe to two endpoints.
        currently bundles will be only addressed to the global one, but this may change.

        we have a map(~crdt-group-endpoint -> map(dtn://node-id -> Dots))

        we store the dot-set for each endpoint, grouped by crdt for faster processing

        on each incoming bundle with a dot-set we temporarily store this dot-set until a forward-request is issued

        on each forward-request we use our temporarily stored dot-set (if available) to compare it to the other known dot-sets in our store,
        thereby determining the nodes to forward our bundle to

  2. which neighbour is our best choice to forward this bundle over for a given destination?

      what do we do:

        we store a score for each destination node per neighbour

        the higher the score, the more likely it is that the bundle will reach the destination node via this neighbour

        structure is like: Map[neighbour: Endpoint, Map[destination_node: Endpoint, score: Long]]

        each received bundle from a neighbour:
          1. increases the score for the bundle source node at that neighbour by 10
          2. decreases the score for the bundle source at all other neighbours by 1
          
          todo: determine the best increase/decrease ratio

        on a forward request, the highest score from all available neighbours is picked

        on equal highest scores, a random neighbour from those highest scores is picked
  */

class RdtRouter(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {
  val deliveryLikelyhoodState: DeliveryLikelyhoodState = DeliveryLikelyhoodState()  // state will grow indefinitely. there is currently no garbage collector

  val destinationDotsState: DestinationDotsState = DestinationDotsState()  // same here
  val neighbourDotsState: NeighbourDotsState = NeighbourDotsState()  // same here

  val tempDotsStore: ConcurrentHashMap[String, Dots] = ConcurrentHashMap()  // maybe grows indefinitely, but only if we receive a bundle which will not be forwarded (hop count depleted?, local unicast endpoint?)
  val tempPreviousNodeStore: ConcurrentHashMap[String, Endpoint] = ConcurrentHashMap()  // same here
  val tempRdtIdStore: ConcurrentHashMap[String, String] = ConcurrentHashMap()  // same here

  val delivered: ConcurrentHashMap[String, Int] = ConcurrentHashMap()  // shows the number of nodes we have delivered a bundle to


  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val source_node: Endpoint = packet.bp.source.extract_node_endpoint()
    val rdt_id: String = packet.bp.destination.extract_endpoint_id()
    val dots: Dots = tempDotsStore.getOrDefault(packet.bp.id, Dots.empty)


    // if we already successfully forwarded this package to enough clas we can 'safely' delete it.
    if (delivered.getOrDefault(packet.bp.id, 0) >= 1000) {
      println("bundle was forwarded to enough unique neighbours. deleting.")
      tempDotsStore.remove(packet.bp.id)
      tempPreviousNodeStore.remove(packet.bp.id)
      tempRdtIdStore.remove(packet.bp.id)
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = true))
    }

    // SHORT-CUT: on no known peers return an empty forwarding request
    if (peers.isEmpty) {
      println("no known peers. returning early with empty response. keeping bundle.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    // get all destination nodes to which we must forward this bundle
    val destination_nodes: Set[Endpoint] = destinationDotsState.getNodeEndpointsToForwardBundleTo(source_node, rdt_id, dots)
    println(s"destination nodes: $destination_nodes")
    
    // for these destination nodes select the ideal neighbours to forward this bundle to
    var ideal_neighbours = destination_nodes
      .map(destination_node => deliveryLikelyhoodState.get_best_neighbours_for(destination_node))
      .flatten
    println(s"ideal neighbours: $ideal_neighbours")

    // remove previous node and source node from ideal neighbours if available
    ideal_neighbours -= source_node
    ideal_neighbours = tempPreviousNodeStore.get(packet.bp.id) match
      case null => ideal_neighbours
      case previous_node: Endpoint => ideal_neighbours - previous_node
    println(s"ideal neighbours without previous and source node: $ideal_neighbours")

    // remove neighbours which already know the state
    ideal_neighbours = neighbourDotsState.filterNeighboursToNotForwardTo(ideal_neighbours, rdt_id, dots)
    println(s"ideal neighbours without neighbours that already know the state: $ideal_neighbours")

    // use current-peers-list to try and get peer information for each ideal neighbour
    val targets: List[DtnPeer] = ideal_neighbours
      .map(x => peers.get(x.extract_node_name()))
      .filter(x => x != null)
      .toList
    println(s"targets: $targets")
    
    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    val selected_clas: List[Sender] = targets
      .map(target => {
        target.cla_list
          .filter((agent, port_option) => packet.clas.contains(agent))
          .map((agent, port_option) => Sender(remote = target.addr, port = port_option, agent = agent, next_hop = target.eid))
      })
      .flatten    
    println(s"selected clas: $selected_clas")

    // if we found at least one cla to forward to then return our forwarding response
    if (!selected_clas.isEmpty) {
      println("clas selected via multicast strategy")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas.toList, delete_afterwards = false))
    }


    // FALLBACK-STRATEGY: on empty cla-list go through all peers in random order until one results in a non-empty cla list
    var filtered_peers: Iterable[DtnPeer] = peers.values().asScala

    println(s"peers: ${List.from(filtered_peers).map(peer => peer.eid)}")

    // remove previous node and source node from peers if available
    filtered_peers = tempPreviousNodeStore.get(packet.bp.id) match
      case null => filtered_peers.filter(p => !p.eid.equals(source_node))
      case previous_node: Endpoint => filtered_peers.filter(p => !p.eid.equals(previous_node) && !p.eid.equals(source_node))
    
    println(s"peers without previous and source node: ${List.from(filtered_peers).map(peer => peer.eid)}")
    
    // remove peers which already know the state
    filtered_peers = neighbourDotsState.filterPeersToNotForwardTo(filtered_peers, rdt_id, dots)

    println(s"peers without neighbours that already know the state: ${List.from(filtered_peers).map(peer => peer.eid)}")


    // choose one peer at random
    return Random
      .shuffle(filtered_peers)
      .map(peer => {
        peer.cla_list
          .filter((agent, port_option) => packet.clas.contains(agent))
          .map((agent, port_option) => Sender(remote = peer.addr, port = port_option, agent = agent, next_hop = peer.eid))
      })
      .collectFirst({
        case head :: next => head :: next
      }) match
        case None => {
          println("could not find any cla to forward to via fallback strategy")
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
        }
        case Some(selected_clas) => {
          println(s"clas selected via fallback strategy: ${selected_clas}")
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = false))
        }
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
    delivered.merge(packet.bid, 1, (x1, x2) => x1 + x2)

    val dots = tempDotsStore.get(packet.bid)
    val rdt_id = tempRdtIdStore.get(packet.bid)

    if (dots != null && rdt_id != null) {
      println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. merging dots.")
      neighbourDotsState.mergeDots(Endpoint.createFromName(packet.cla_sender), rdt_id, dots)
    } else {
      println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. could not merge dots because dots or rdt_id are no longer available in temp-store.")
    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    // prevent unnecessary merging on cyclic forwarding: only update the scores (and dots, although this is only prevents processing time wasting), if we see a packet for the first time.
    // if we have already seen the bundle, it must either be delivered, or we have extracted information and put the in, for example, the dots store
    if (delivered.containsKey(packet.bndl.id) || tempDotsStore.containsKey(packet.bndl.id)) {
      println("received bundle which was already seen. not updating scores.")
      return
    }

    packet.bndl.other_blocks.collectFirst{
      case x: PreviousNodeBlock => x
    } match
      case None => println("received incoming bundle without previous node block. ignoring")
      case Some(previous_node_block) => {
        val source_node = packet.bndl.primary_block.source.extract_node_endpoint()
        val previous_node = previous_node_block.previous_node_id

        println(s"received incoming bundle with previous node block. source node ${source_node}. previous node ${previous_node}. increasing score.")

        deliveryLikelyhoodState.update_score(neighbour_node = previous_node, destination_node = source_node)

        tempPreviousNodeStore.put(packet.bndl.id, previous_node)
      }
    
    packet.bndl.other_blocks.collectFirst{
      case x: RdtMetaBlock => x
    } match
      case None => println("received incoming bundle without rdt-meta block. ignoring")
      case Some(rdt_meta_block) => {
        val source_node_endpoint = packet.bndl.primary_block.source.extract_node_endpoint()
        val rdt_id = packet.bndl.primary_block.destination.extract_endpoint_id()

        println(s"received incoming bundle ${packet.bndl.id} with rdt-meta block. merging.")

        // we can already merge here because the source is obviously excluded from the forwarding destinations selection for this bundle
        // and the information is already available for other maybe earlier forwarding requests
        destinationDotsState.mergeDots(source_node_endpoint, rdt_id, rdt_meta_block.dots)

        tempDotsStore.put(packet.bndl.id, rdt_meta_block.dots)
        tempRdtIdStore.put(packet.bndl.id, rdt_id)  // only needed in combination with the dots on successful forward, so add it here
      }
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    // todo: if bundle on IncomingBundle is stored temporarily, then use that info to increase the score
    // currently irrelevant, as this message is never sent by the dtnd
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object RdtRouter {
  def apply(host: String, port: Int): Future[RdtRouter] = WSEroutingClient(host, port).map(ws => new RdtRouter(ws))
}


class DeliveryLikelyhoodState {
  // structure: map[neighbour-node, map[destination-node, score]]
  val map: ConcurrentHashMap[Endpoint, ConcurrentHashMap[Endpoint, Long]] = ConcurrentHashMap()

  def update_score(neighbour_node: Endpoint, destination_node: Endpoint): Unit = {
    map.putIfAbsent(neighbour_node, ConcurrentHashMap())

    // increase score by 10 for this node combination (gets decremented by 1 in the next step)
    map.get(neighbour_node).merge(destination_node, 11, (x1, x2) => Try(addExact(x1, x2)).getOrElse(Long.MaxValue))

    // decrease score by 1 for all other node combinations with that destination
    map.values().forEach(destination_map => {
      if (destination_map.containsKey(destination_node)) {  // only merge if that combination was already seen to save space
        destination_map.merge(destination_node, -1, (x1, x2) => max(0, x1 + x2))  // lowest value is 0
      }
    })
  }

  def get_best_neighbours_for(destination_node: Endpoint): Set[Endpoint] = {
    var highscore: Long = 0
    var highscore_nodes: Set[Endpoint] = Set()

    for((neighbour, destination_map) <- map.asScala) {  // should be fine because we only read here
      val score: Long = destination_map.getOrDefault(destination_node, 0)

      if (score == highscore) {
        highscore_nodes += neighbour
      } else if (score > highscore) {
        highscore = score
        highscore_nodes = Set(neighbour)
      }
    }

    highscore_nodes  // todo: is there a useful max? should we only include the best score or also lower scores for more neighbours?
  }
}


class DestinationDotsState {
  // structure: map[rdt-group-id, map[node-id, Dots]]
  val map: ConcurrentHashMap[String, ConcurrentHashMap[Endpoint, Dots]] = ConcurrentHashMap()

  /* 
    adds/merges this nodes' dots to the data structure.
  */
  def mergeDots(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Unit = {
    map.putIfAbsent(rdt_id, ConcurrentHashMap())
    map.get(rdt_id).merge(node_endpoint, dots, (x1, x2) => x1.merge(x2))
  }

  /* 
    finds all nodes for which this nodes' dots are bigger than the other nodes' dots
  */
  def getNodeEndpointsToForwardBundleTo(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Set[Endpoint] = {
    map.get(rdt_id) match
      case null => Set()
      case rdt_map: ConcurrentHashMap[Endpoint, Dots] => {
        rdt_map.asScala
          .filter((n: Endpoint, d: Dots) => !n.equals(node_endpoint) && !(dots <= d))
          .collect[Endpoint]((n: Endpoint, d: Dots) => n)
          .toSet
      }
  }
}


class NeighbourDotsState {
  // structure: map[rdt-group-id, map[node-id, Dots]]
  val map: ConcurrentHashMap[String, ConcurrentHashMap[Endpoint, Dots]] = ConcurrentHashMap()

  /* 
    adds/merges this nodes' dots to the data structure.
  */
  def mergeDots(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Unit = {
    map.putIfAbsent(rdt_id, ConcurrentHashMap())
    map.get(rdt_id).merge(node_endpoint, dots, (x1, x2) => x1.merge(x2))
  }

  /* 
    return all neighbours for which the provided dots are not already known to them
  */
  def filterNeighboursToNotForwardTo(neighbour_node_endpoints: Set[Endpoint], rdt_id: String, dots: Dots): Set[Endpoint] = {
    neighbour_node_endpoints.filter(endpoint => {
      val d = map.get(rdt_id) match
        case null => Dots.empty
        case node_map: ConcurrentHashMap[Endpoint, Dots] => node_map.getOrDefault(endpoint, Dots.empty)

      !(dots <= d) || dots.isEmpty
    })
  }

  def filterPeersToNotForwardTo(peers: Iterable[DtnPeer], rdt_id: String, dots: Dots): Iterable[DtnPeer] = {
    peers.filter(peer => {
      val d = map.get(rdt_id) match
        case null => Dots.empty
        case node_map: ConcurrentHashMap[Endpoint, Dots] => node_map.getOrDefault(peer.eid, Dots.empty)
      
      !(dots <= d) || dots.isEmpty
    })
  }
}
