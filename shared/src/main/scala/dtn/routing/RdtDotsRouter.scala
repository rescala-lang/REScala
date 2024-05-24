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

class RdtDotsRouter(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {
  // all these states will grow indefinitely. there is currently no garbage collector

  val deliveryLikelyhoodState: DeliveryLikelyhoodState = DeliveryLikelyhoodState()

  val tempDotsStore: mutable.Map[String, Dots] = mutable.Map()  // maybe grows indefinitely, but only if we receive a bundle which will not be forwarded (hop count depleted?, local unicast endpoint?)
  val tempPreviousNodeStore: mutable.Map[String, Endpoint] = mutable.Map()  // same here

  val destinationDotsState: DestinationDotsState = DestinationDotsState()

  val delivered: mutable.Set[String] = mutable.Set()


  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    val source_node: Endpoint = packet.bp.source.extract_node_endpoint()

    // CYCLIC FORWARDING PREVENTION: if we already successfully forwarded this package to some node we can safely delete it.
    if (delivered.contains(packet.bp.id)) {
      println("bundle was already seen and delivered. throwing away.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = true))
    }

    // SHORT-CUT: on no known peers return an empty forwarding request
    if (peers.isEmpty) {
      println("no known peers. returning early with empty response. keeping bundle.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    // get all destination nodes to which we must forward this bundle
    val destination_nodes: Set[Endpoint] = tempDotsStore.get(packet.bp.id) match
      case None => Set()
      case Some(d) => {
        val rdt_id = packet.bp.destination.extract_endpoint_id()
        destinationDotsState.getNodeEndpointsToForwardBundleTo(source_node, rdt_id, d)
      }
    println(s"destination nodes: $destination_nodes")
    
    // for these destination nodes select the ideal neighbours to forward this bundle to
    val ideal_neighbours: mutable.Set[Endpoint] = mutable.Set()
    for (destination_node <- destination_nodes) {
      ideal_neighbours ++= deliveryLikelyhoodState.get_best_neighbours_for(destination_node)
    }
    println(s"ideal neighbours: $ideal_neighbours")

    // remove previous node and source node from ideal neighbours if available
    ideal_neighbours.remove(source_node)
    tempPreviousNodeStore.get(packet.bp.id) match
      case None => {}
      case Some(previous_node) => ideal_neighbours.remove(previous_node)

    println(s"ideal neighbours without previous and source node: $ideal_neighbours")

    // use current-peers-list to try and get peer information for each ideal neighbour
    val targets: List[DtnPeer] = ideal_neighbours
      .map[Option[DtnPeer]](x => peers.get(x.extract_node_name()))
      .collect({ case Some(value) => value })
      .toList
    println(s"targets: $targets")
    
    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    var selected_clas: mutable.ListBuffer[Sender] = mutable.ListBuffer()
    for (target <- targets) {
      for ((cla_agent, cla_port) <- target.cla_list) {
        if (packet.clas.contains(cla_agent)) {
          selected_clas += Sender(remote = target.addr, port = cla_port, agent = cla_agent, next_hop = target.eid)
        }
      }
    }
    println(s"selected clas: $selected_clas")

    // if we found at least one cla to forward to then return our forwarding response
    if (!selected_clas.isEmpty) {
      println("clas selected via multicast strategy")
      delivered += packet.bp.id
      tempDotsStore.remove(packet.bp.id)
      tempPreviousNodeStore.remove(packet.bp.id)
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas.toList, delete_afterwards = true))
    }

    // FALLBACK-STRATEGY: on empty cla-list go through all peers in random order until one results in a non-empty cla list
    // if present, filter out the previous node and source node from all available peers
    val filtered_peers: Iterable[DtnPeer] = tempPreviousNodeStore.get(packet.bp.id) match
      case None => peers.values.filter(p => !p.eid.equals(source_node))
      case Some(previous_node) => peers.values.filter(p => !p.eid.equals(previous_node) && !p.eid.equals(source_node))

    Random.shuffle(filtered_peers)
      .map[List[Sender]](peer => {
        val clas: mutable.ListBuffer[Sender] = mutable.ListBuffer()
        for ((cla_agent, cla_port) <- peer.cla_list) {
          if (packet.clas.contains(cla_agent)) {
            clas += Sender(remote = peer.addr, port = cla_port, agent = cla_agent, next_hop = peer.eid)
          }
        }
        clas.toList
      })
      .collectFirst({
        case head :: next => head :: next
      }) match
        case None => {
          println("could not find any cla to forward to via fallback strategy")
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
        }
        case Some(clas) => {
          println("clas selected via fallback strategy")
          delivered += packet.bp.id
          tempDotsStore.remove(packet.bp.id)
          tempPreviousNodeStore.remove(packet.bp.id)
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = clas, delete_afterwards = true))
        }
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp.id}. doing nothing.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    delivered -= packet.bid
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}. removing delivered state.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. doing nothing.")
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    // CYCLIC FORWARDING PREVENTION: only update the scores (and dots, although this is only prevents processing time wasting), if we see a packet for the first time. todo: check if a packet may be received from multiple locations without being delivered.
    if (delivered.contains(packet.bndl.id)) {
      println("received bundle which was already delivered. not updating scores.")
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

        deliveryLikelyhoodState.update_score(neighbour = previous_node, destination_node = source_node)

        tempPreviousNodeStore += (packet.bndl.id -> previous_node)
      }
    
    packet.bndl.other_blocks.collectFirst{
      case x: RdtMetaBlock => x
    } match
      case None => println("received incoming bundle without rdt-meta block. ignoring")
      case Some(rdt_meta_block) => {
        val bid = packet.bndl.id
        val dots = rdt_meta_block.dots

        println(s"received incoming bundle ${bid} with rdt-meta block. merging.")

        // we can already merge here because the source is obviously excluded from the forwarding destinations selection for this bundle
        // and the information is already available for other maybe earlier forwarding requests
        val source_node_endpoint = packet.bndl.primary_block.source.extract_node_endpoint()
        val rdt_id = packet.bndl.primary_block.destination.extract_endpoint_id()
        destinationDotsState.mergeDots(source_node_endpoint, rdt_id, dots)

        tempDotsStore += (bid -> dots)
      }
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    // todo: if bundle on IncomingBundle is stored temporarily, then use that info to increase the score
    // currently irrelevant, as this message is never sent by the dtnd
    println(s"received incoming bundle without previous node. information not used for routing. ignoring. message: $packet")
  }
}
object RdtDotsRouter {
  def apply(host: String, port: Int): Future[RdtDotsRouter] = WSEroutingClient(host, port).map(ws => new RdtDotsRouter(ws))
}


class DeliveryLikelyhoodState {
  val neighbourLikelyhoodMap: mutable.Map[Endpoint, mutable.Map[Endpoint, Long]] = mutable.Map()

  def update_score(neighbour: Endpoint, destination_node: Endpoint): Unit = {
    var old_score: Long = 0;

    neighbourLikelyhoodMap.get(neighbour) match
      case None => neighbourLikelyhoodMap(neighbour) = mutable.Map()
      case Some(value) => old_score = value.getOrElse(destination_node, 0)
    
    if (Long.MaxValue - old_score < 11) {
      neighbourLikelyhoodMap(neighbour)(destination_node) = Long.MaxValue  // highest value (minus one in the next step, i dont care) is Long.MaxValue
    } else {
      neighbourLikelyhoodMap(neighbour)(destination_node) = old_score + 11  // gets decreased by one in the next loop
    }

    for (destination_map <- neighbourLikelyhoodMap.values) {
      destination_map.get(destination_node) match
        case None => {}
        case Some(value) => destination_map(destination_node) = if (value <= 0) 0 else value-1  // lowest value is 0
    }
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

    highscore_nodes  // todo: is there a useful max? should be aim for that max with including lower score nodes?
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
      case Some(rdt_map) => rdt_map += node_endpoint -> rdt_map.getOrElse(node_endpoint, Dots.empty).merge(dots)
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

