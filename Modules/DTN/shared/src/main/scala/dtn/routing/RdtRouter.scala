package dtn.routing

import dtn.{DtnPeer, Endpoint, Packet, PreviousNodeBlock, RdtMetaBlock, Sender, WSEroutingClient}
import rdts.time.Dots

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.math.{addExact, max}
import scala.util.{Random, Try}
import dtn.RdtMetaInfo

// Variables, chosen number, for this routing
val MIN_DELIVERED    = 10
val N_TOP_NEIGHBOURS = 3

/*
  We need to determine two things in this router for a given bundle:

  1. to which destinations must we forward this bundle?

      what do we do:

        To be able to merge source-node-information, the bundle-source will be a unicast endpoint like: dtn://node-id/rdt/app-name
        In contrast to the bundle-destination, which will be a group endpoint like: dtn://global/~rdt/app-name

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

        structure is like: Map[destination_node: Endpoint, Map[neighbour_node: Endpoint, score: Long]]

        each received bundle from a neighbour:
          1. increases the score for the destination=bundle-source-node and neighbour=bundle-previous-node by 1
          2. normalizes scores over all combinations with that destination

        on a forward request, the sorted list with the highest score first is returned. the first n neighbours are picked.
 */

class RdtRouter(ws: WSEroutingClient) extends BaseRouter(ws: WSEroutingClient) {
  // epidemic routing is for rdt-request-bundles. they do not contain any payload that contributes to our state (in contrast to rdt-payload-bundles)
  val epidemicStrategy: EpidemicStrategy = EpidemicStrategy()

  // state will grow indefinitely. there is currently no garbage collector
  val likelihoodState: LikelihoodState = LikelihoodState()
  val dotState: DotState               = DotState()

  // maybe grows indefinitely, but only if we receive a bundle which will not be forwarded (hop count depleted?, local unicast endpoint?)
  val tempRdtMetaInfoStore: ConcurrentHashMap[String, RdtMetaInfo] = ConcurrentHashMap()
  val tempPreviousNodeStore: ConcurrentHashMap[String, Endpoint]   = ConcurrentHashMap()
  val tempRdtIdStore: ConcurrentHashMap[String, String]            = ConcurrentHashMap()

  val delivered: ConcurrentHashMap[String, Int] =
    ConcurrentHashMap() // shows the number of nodes we have delivered a bundle to

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    // we only route rdt packets rn
    if !tempRdtMetaInfoStore.contains(packet.bp.id) then {
      println(s"bundle meta information for bundle-id ${packet.bp.id} are not available. not routing rn.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    val source_node: Endpoint      = packet.bp.source.extract_node_endpoint()
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

    // if we already successfully forwarded this package to enough clas we can 'safely' delete it.
    if delivered.getOrDefault(packet.bp.id, 0) >= MIN_DELIVERED then {
      println("bundle was forwarded to enough unique neighbours. deleting.")
      tempRdtMetaInfoStore.remove(packet.bp.id)
      tempPreviousNodeStore.remove(packet.bp.id)
      tempRdtIdStore.remove(packet.bp.id)
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = true))
    }

    // SHORT-CUT: on no known peers return an empty forwarding request
    if peers.isEmpty then {
      println("no known peers. returning early with empty response. keeping bundle.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    // get all destination nodes to which we must forward this bundle
    val destination_nodes: Set[Endpoint] =
      dotState.getDestinationNodeEndpoints(source_node, rdt_id, rdt_meta_info.dots)
    println(s"destination nodes: $destination_nodes")

    // for these destination nodes select the best neighbours to forward this bundle to
    var best_neighbours = destination_nodes.flatMap(node => likelihoodState.get_sorted_neighbours(node).take(5))
    println(s"best neighbours: $best_neighbours")

    // remove previous node and source node from ideal neighbours if available
    // todo: check if this is really necessary or not already covered by the next step
//    best_neighbours -= source_node
//    best_neighbours = tempPreviousNodeStore.get(packet.bp.id) match
//      case null                    => best_neighbours
//      case previous_node: Endpoint => best_neighbours - previous_node
//    println(s"best neighbours without previous and source node: $best_neighbours")

    // remove neighbours which already know the state
    best_neighbours = dotState.filterNeighbourNodes(best_neighbours, rdt_id, rdt_meta_info.dots)
    println(s"best neighbours without neighbours that already know the state: $best_neighbours")

    // use current-peers-list to try and get peer information for each best neighbour
    var targets: List[DtnPeer] = best_neighbours
      .map(x => peers.get(x.extract_node_name()))
      .filter(x => x != null)
      .toList
    println(s"multicast targets: $targets")

    if targets.size < 3 then {
      println("multicast targets are less than 3, trying to add random peers from fallback strategy")

      var random_peers: Iterable[DtnPeer] = peers.values().asScala

      // remove peers that are already in the multicast selection
      random_peers = random_peers.filter(p => !targets.contains(p))

      // remove previous node and source node from peers if available
      random_peers = tempPreviousNodeStore.get(packet.bp.id) match
        case null => random_peers.filter(p => !p.eid.equals(source_node))
        case previous_node: Endpoint =>
          random_peers.filter(p => !p.eid.equals(previous_node) && !p.eid.equals(source_node))

      println(s"filtered random peers available: ${List.from(random_peers).map(peer => peer.eid)}")

      targets = targets ++ Random.shuffle(random_peers).take(N_TOP_NEIGHBOURS - targets.size)
    }

    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    val selected_clas: List[Sender] = targets.flatMap(target => {
      target.cla_list
        .filter((agent, port_option) => packet.clas.contains(agent))
        .map((agent, port_option) =>
          Sender(remote = target.addr, port = port_option, agent = agent, next_hop = target.eid)
        )
    })
    println(s"selected clas: $selected_clas")

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
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp.id}.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    val rdt_meta_info = tempRdtMetaInfoStore.get(packet.bid)
    val rdt_id        = tempRdtIdStore.get(packet.bid)

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
          delivered.merge(packet.bid, 1, (x1, x2) => x1 + x2)
          println("rdt-payload-message. merging dots")
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
      println(s"received incoming bundle ${packet.bndl.id} with rdt-meta block and previous-node block.")

      rdt_meta_info.get.message_type match
        case RdtMessageType.Request => {
          println("rdt-request-message. not merging")
        }
        case RdtMessageType.Payload => {
          println("rdt-payload-message. merging")
          likelihoodState.update_score(neighbour_node = previous_node.get, destination_node = source_node)
          dotState.mergeDots(source_node, rdt_id, rdt_meta_info.get.dots)
          dotState.mergeDots(previous_node.get, rdt_id, rdt_meta_info.get.dots)
        }

      tempPreviousNodeStore.put(packet.bndl.id, previous_node.get)
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
object RdtRouter {
  def apply(host: String, port: Int): Future[RdtRouter] = WSEroutingClient(host, port).map(ws => new RdtRouter(ws))
}

class LikelihoodState {
  // structure: map[destination-node, map[neighbour-node, score]]
  val map: ConcurrentHashMap[Endpoint, Map[Endpoint, Double]] = ConcurrentHashMap()

  /*
    adds 1 to the score of this destination + neighbour combination
    normalizes scores to 1 over all combinations with that destination
   */
  def update_score(destination_node: Endpoint, neighbour_node: Endpoint): Unit = {
    var m = map.getOrDefault(destination_node, Map())

    m = m.updated(neighbour_node, m.getOrElse(neighbour_node, 0.0) + 1.0)

    val sum = m.values.sum

    m = m.map { case (neighbour_node, score) => neighbour_node -> score / sum }

    map.put(destination_node, m)
    ()
  }

  /*
    returns all known neighbours for a destination in a set, sorted by delivery-likelihood, with the best neighbour (highest score) first
   */
  def get_sorted_neighbours(destination_node: Endpoint): Set[Endpoint] = {
    map
      .get(destination_node)
      .toList
      .sortBy(-_._2)
      .map(_._1)
      .toSet
  }
}

class DotState {
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
    finds all nodes for which this nodes' dots are bigger than the other nodes' dots
   */
  def getDestinationNodeEndpoints(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Set[Endpoint] = {
    map.get(rdt_id) match
      case null => Set()
      case rdt_map: ConcurrentHashMap[Endpoint, Dots] =>
        rdt_map.asScala
          .filter((n: Endpoint, d: Dots) => !n.equals(node_endpoint) && !(dots <= d))
          .collect[Endpoint]((n: Endpoint, d: Dots) => n)
          .toSet
  }

  /*
    return all neighbours for which the provided dots are not already known to them
   */
  def filterNeighbourNodes(neighbour_node_endpoints: Set[Endpoint], rdt_id: String, dots: Dots): Set[Endpoint] = {
    neighbour_node_endpoints.filter(endpoint => {
      val d = map.get(rdt_id) match
        case null                                        => Dots.empty
        case node_map: ConcurrentHashMap[Endpoint, Dots] => node_map.getOrDefault(endpoint, Dots.empty)

      !(dots <= d) || dots.isEmpty
    })
  }
}
