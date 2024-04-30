package dtn.routers

import dtn.{BaseRouter, DtnPeer, Packet, Sender, WSEroutingClient}
import scala.collection.mutable.{ListBuffer, Map}
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

        the endpoint naming scheme is: dtn://node-id/~crdt-group-endpoint
        but only for the source endpoint. the destination endpoint will be a global one, e.g. with no information on the destination-node list whatsoever

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

class RdtDotsRouter extends BaseRouter {
  val deliveryLikelyhoodState: DeliveryLikelyhoodState = DeliveryLikelyhoodState()

  val tempBundleIdToDotSetMap: Map[String, Dots] = Map()  // todo: check if this grows indefinitely

  val destinationDotsState: DestinationDotsState = DestinationDotsState()


  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle): Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    // SHORT-CUT: on no known peers return an empty forwarding request
    if (peers.isEmpty) {
      println("no known peers. returning early with empty response.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    // get all destination nodes to which we must forward this bundle
    val destination_nodes: Set[Endpoint] = tempBundleIdToDotSetMap.get(packet.bp.id) match
      case None => Set()
      case Some(dots) => {
        destinationDotsState.getNodeEndpointsToForwardBundleTo(packet.bp.source, dots)
      }
    
    // for these destination nodes select the ideal neighbours to forward this bundle to
    var ideal_neighbours: Set[Endpoint] = Set()
    for (destination_node <- destination_nodes) {
      ideal_neighbours ++= deliveryLikelyhoodState.get_best_neighbours_for(destination_node)
    }

    // use current-peers-list to try and get peer information for each ideal neighbour
    val targets: List[DtnPeer] = ideal_neighbours
      .map[Option[DtnPeer]](x => peers.get(x.extract_node_name()))
      .collect({ case Some(value) => value })
      .toList
    
    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    var selected_clas: ListBuffer[Sender] = ListBuffer()
    for (target <- targets) {
      for ((cla_agent, cla_port) <- target.cla_list) {
        if (packet.clas.contains(cla_agent)) {
          selected_clas += Sender(remote = target.addr, port = cla_port, agent = cla_agent, next_hop = target.eid)
        }
      }
    }

    // if we found at least one cla to forward to then return our forwarding response
    if (!selected_clas.isEmpty) {
      println("clas selected via multicast strategy")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = selected_clas.toList, delete_afterwards = true))
    }

    // FALLBACK-STRATEGY: on empty cla-list go through all peers in random order until one results in a non-empty cla list
    Random.shuffle(peers.values)
      .map[List[Sender]](peer => {
        var clas: ListBuffer[Sender] = ListBuffer()
        for ((cla_agent, cla_port) <- peer.cla_list) {
          if (packet.clas.contains(cla_agent)) {
            selected_clas += Sender(remote = peer.addr, port = cla_port, agent = cla_agent, next_hop = peer.eid)
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
          Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = clas, delete_afterwards = true))
        }
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp}. doing nothing.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}. doing nothing.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    println(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. doing nothing.")
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    packet.bndl.other_blocks.collectFirst{
      case x: PreviousNodeBlock => x
    } match
      case None => println("received incoming bundle without previous node block. ignoring")
      case Some(previous_node_block) => {
        val source_node = packet.bndl.primary_block.source.extract_node_endpoint()
        val previous_node = previous_node_block.previous_node_id

        println(s"received incoming bundle with previous node block. source node ${source_node}. previous node ${previous_node}. increasing score.")

        deliveryLikelyhoodState.update_score(neighbour = previous_node, destination_node = source_node)
      }
    
    packet.bndl.other_blocks.collectFirst{
      case x: RdtMetaBlock => x
    } match
      case None => println("received incoming bundle without rdt-meta block. ignoring")
      case Some(rdt_meta_block) => {
        val bid = packet.bndl.id
        val dots = rdt_meta_block.dots

        println(s"received incoming bundle ${bid} with rdt-meta block. dots ${dots}. merging and temp storing for forwarding request")

        // we can already merge here because the source is obviously excluded from the forwarding destinations selection for this bundle
        // and the information is already available for other maybe earlier forwarding requests
        destinationDotsState.mergeDots(packet.bndl.primary_block.source, dots)
        tempBundleIdToDotSetMap += (bid -> dots)
      }
    
    
    // todo: collectFirst new DotSetBlock, add bundle-id -> Dots to tempBundleIdToDotSetMap
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    // todo: if bundle on IncomingBundle is stored temporarily, then use that info to increase the score
    // currently irrelevant, as this message is never sent by the dtnd
    println(s"received incoming bundle without previous node. information not used for routing. ignoring. message: $packet")
  }
}
object RdtDotsRouter {
  def create(port: Int): Future[RdtDotsRouter] = {
    val router = RdtDotsRouter()

    WSEroutingClient.create(port).map(ws => {
      router.ws = Option(ws)
      router
    })
  }
}


class DeliveryLikelyhoodState {
  val neighbourLikelyhoodMap: Map[Endpoint, Map[Endpoint, Long]] = Map()

  def update_score(neighbour: Endpoint, destination_node: Endpoint): Unit = {
    var old_score: Long = 0;

    neighbourLikelyhoodMap.get(neighbour) match
      case None => neighbourLikelyhoodMap(neighbour) = Map()
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
  val map: Map[String, Map[Endpoint, Dots]] = Map()

  /* 
    adds/merges this nodes' dots to the data structure.
  */
  def mergeDots(endpoint: Endpoint, dots: Dots): Unit = {
    val node_endpoint = endpoint.extract_node_endpoint()
    val rdt_id = endpoint.extract_endpoint_id()

    map.get(rdt_id) match
      case None => map(rdt_id) = Map(node_endpoint -> dots)
      case Some(rdt_map) => rdt_map += node_endpoint -> rdt_map.getOrElse(node_endpoint, Dots.empty).merge(dots)
  }

  /* 
    finds all nodes for which this nodes' dots are bigger than the other nodes' dots
  */
  def getNodeEndpointsToForwardBundleTo(endpoint: Endpoint, dots: Dots): Set[Endpoint] = {
    val node_endpoint = endpoint.extract_node_endpoint()
    val rdt_id = endpoint.extract_endpoint_id()

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

