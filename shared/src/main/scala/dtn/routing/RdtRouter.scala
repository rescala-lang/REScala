package dtn.routing

import dtn.{Bundle, DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import dtn.Endpoint
import dtn.PreviousNodeBlock
import kofre.time.Dots
import dtn.RdtMetaBlock
import scala.util.Random
import math.max
import scala.jdk.CollectionConverters._


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

  val tempDotsStore: mutable.Map[String, Dots] = mutable.Map()  // maybe grows indefinitely, but only if we receive a bundle which will not be forwarded (hop count depleted?, local unicast endpoint?)
  val tempPreviousNodeStore: mutable.Map[String, Endpoint] = mutable.Map()  // same here
  val tempRdtIdStore: mutable.Map[String, String] = mutable.Map()  // same here

  val delivered: mutable.Map[String, Int] = mutable.Map()  // shows the number of nodes we have delivered a bundle to


  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val source_node: Endpoint = packet.bp.source.extract_node_endpoint()
    val rdt_id: String = packet.bp.destination.extract_endpoint_id()
    val dots: Dots = tempDotsStore.getOrElse(packet.bp.id, Dots.empty)


    // if we already successfully forwarded this package to enough clas we can 'safely' delete it.
    if (delivered.getOrElse(packet.bp.id, 0) >= 1000) {
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
      case None => ideal_neighbours
      case Some(previous_node) => ideal_neighbours - previous_node
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
      delivered.put(packet.bp.id, delivered.getOrElse(packet.bp.id, 0) + selected_clas.size)
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas.toList, delete_afterwards = false))
    }

    // if we did not forward this bundle at all so far do the fallback strategy
    if (delivered.getOrElse(packet.bp.id, 0) <= 0) {
      // FALLBACK-STRATEGY: on empty cla-list go through all peers in random order until one results in a non-empty cla list
      // if present, filter out the previous node and source node from all available peers
      val filtered_peers: Iterable[DtnPeer] = tempPreviousNodeStore.get(packet.bp.id) match
      case None => peers.values.asScala.filter(p => !p.eid.equals(source_node))
      case Some(previous_node) => peers.values.asScala.filter(p => !p.eid.equals(previous_node) && !p.eid.equals(source_node))

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
            println("clas selected via fallback strategy")
            delivered.put(packet.bp.id, delivered.getOrElse(packet.bp.id, 0) + selected_clas.size)
            Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas, delete_afterwards = false))
          }
    }
    
    // else wait for new peers
    println("forwarded at least once. waiting for new peers.")
    return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp.id}.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    delivered.put(packet.bid, delivered.getOrElse(packet.bid, 0) - 1)
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    val dots = tempDotsStore.get(packet.bid)
    val rdt_id = tempRdtIdStore.get(packet.bid)

    if (dots.isDefined && rdt_id.isDefined) {
      println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. merging dots.")
      neighbourDotsState.mergeDots(Endpoint.createFromName(packet.cla_sender), rdt_id.get, dots.get)
    } else {
      println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. could not merge dots because dots or rdt_id are no longer available in temp-store.")
    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    // prevent unnecessary merging on cyclic forwarding: only update the scores (and dots, although this is only prevents processing time wasting), if we see a packet for the first time.
    // if we have already seen the bundle, it must either be delivered, or we have extracted information and put the in, for example, the dots store
    if (delivered.contains(packet.bndl.id) || tempDotsStore.contains(packet.bndl.id)) {
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

        tempPreviousNodeStore += (packet.bndl.id -> previous_node)
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

        tempDotsStore += (packet.bndl.id -> rdt_meta_block.dots)
        tempRdtIdStore += (packet.bndl.id -> rdt_id)  // only needed in combination with the dots on successful forward, so add it here
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
  var neighbourLikelyhoodMap: Map[Endpoint, mutable.Map[Endpoint, Long]] = Map()

  def update_score(neighbour_node: Endpoint, destination_node: Endpoint): Unit = {
    if (!neighbourLikelyhoodMap.contains(neighbour_node)) {
      neighbourLikelyhoodMap += (neighbour_node -> mutable.Map())
    }

    // increase score by 10 for this node combination    
    val old_score: Long = neighbourLikelyhoodMap(neighbour_node).getOrElse(destination_node, 0)

    if (Long.MaxValue - old_score < 11) {
      neighbourLikelyhoodMap(neighbour_node)(destination_node) = Long.MaxValue  // highest value (minus one in the next step, i dont care) is Long.MaxValue
    } else {
      neighbourLikelyhoodMap(neighbour_node)(destination_node) = old_score + 11  // gets decreased by one in the next loop
    }

    // decrease score by 1 for all other node combinations with that destination
    neighbourLikelyhoodMap
      .values
      .foreach(destination_map => {
        destination_map.get(destination_node) match
          case None => ()
          case Some(value) => destination_map(destination_node) = max(0, value-1)  // lowest value is 0
      })
  }

  def get_best_neighbours_for(destination_node: Endpoint): Set[Endpoint] = {
    var highscore: Long = 0
    var highscore_nodes: Set[Endpoint] = Set()

    for((neighbour, destination_map) <- neighbourLikelyhoodMap) {
      val score: Long = destination_map.getOrElse(destination_node, 0)

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
  val map: mutable.Map[String, mutable.Map[Endpoint, Dots]] = mutable.Map()

  /* 
    adds/merges this nodes' dots to the data structure.
  */
  def mergeDots(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Unit = {
    map.get(rdt_id) match
      case None => map(rdt_id) = mutable.Map(node_endpoint -> dots)
      case Some(rdt_map) => rdt_map += (node_endpoint -> rdt_map.getOrElse(node_endpoint, Dots.empty).merge(dots))
  }

  /* 
    finds all nodes for which this nodes' dots are bigger than the other nodes' dots
  */
  def getNodeEndpointsToForwardBundleTo(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Set[Endpoint] = {
    map.get(rdt_id) match
      case None => Set()
      case Some(rdt_map) => {
        rdt_map
          .filter((n: Endpoint, d: Dots) => !n.equals(node_endpoint) && !(dots <= d))
          .collect[Endpoint]((n: Endpoint, d: Dots) => n)
          .toSet
      }
  }
}


class NeighbourDotsState {
  // structure: map[rdt-group-id, map[node-id, Dots]]
  val map: mutable.Map[String, mutable.Map[Endpoint, Dots]] = mutable.Map()

  /* 
    adds/merges this nodes' dots to the data structure.
  */
  def mergeDots(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Unit = {
    map.get(rdt_id) match
      case None => map(rdt_id) = mutable.Map(node_endpoint -> dots)
      case Some(rdt_map) => rdt_map += (node_endpoint -> rdt_map.getOrElse(node_endpoint, Dots.empty).merge(dots))
  }

  /* 
    return all neighbours for which the provided dots are not already known to them
  */
  def filterNeighboursToNotForwardTo(neighbour_node_endpoints: Set[Endpoint], rdt_id: String, dots: Dots): Set[Endpoint] = {
    neighbour_node_endpoints.filter(endpoint => {
      val d = map.get(rdt_id) match
        case None => Dots.empty
        case Some(node_map) => node_map.getOrElse(endpoint, Dots.empty)

      !(dots <= d)
    })
  }
}

