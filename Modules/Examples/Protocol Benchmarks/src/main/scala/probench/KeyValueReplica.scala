package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.RequestResponseQueue.Req
import probench.data.{ClientState, ClusterState, KVOperation, RequestResponseQueue}
import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{MultiPaxos, MultipaxosPhase, Participants}
import replication.DeltaDissemination
import probench.Codecs.given

import java.util.concurrent.{ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.ExecutionContext

class KeyValueReplica(val uid: Uid, val votingReplicas: Set[Uid]) {

  val timer = new java.util.Timer()

  inline def log(inline msg: String): Unit =
    if false then println(s"[$uid] $msg")

  given Participants(votingReplicas)

  given localUid: LocalUid = LocalUid(uid)

  val currentStateLock: AnyRef   = new {}
  var clusterState: ClusterState = MultiPaxos.empty

  timer.schedule(
    () => {
      println(s"[$uid] current state ${clusterState.hashCode()}")
    },
    1000,
    1000
  )

  var clientState: ClientState = RequestResponseQueue.empty

  val sendingActor: ExecutionContext = {

    val singleThreadExecutor: ExecutorService = Executors.newSingleThreadExecutor(r => {
      val thread = new Thread(r)
      thread.setDaemon(true)
      thread
    })

    ExecutionContext.fromExecutorService(singleThreadExecutor)
  }

  val clusterDataManager: DeltaDissemination[ClusterState] =
    DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = true,
      sendingActor = sendingActor
    )
  val clientDataManager: DeltaDissemination[ClientState] =
    DeltaDissemination(
      localUid,
      handleClientStateChange,
      immediateForward = true,
      sendingActor = sendingActor
    )

  // propose myself as leader if I have the lowest id
  votingReplicas.minOption match
    case Some(id) if id == uid =>
      transformCluster(_.startLeaderElection)
    case _ => ()

  def publish(delta: ClusterState): ClusterState = currentStateLock.synchronized {
    if delta `inflates` clusterState then {
      log(s"publishing")
      clusterState = clusterState.merge(delta)
      clusterDataManager.applyDelta(delta)
    } else
      log(s"skip")
    clusterState
  }

  def publishClient(delta: ClientState): ClientState = currentStateLock.synchronized {
    if delta `inflates` clientState then {
      log(s"publishing")
      clientState = clientState.merge(delta)
      clientDataManager.applyDelta(delta)
    } else
      log(s"skip")
    clientState
  }

  def forceUpkeep(): ClusterState = currentStateLock.synchronized {
    publish(clusterState.upkeep)
  }

  def needsUpkeep(): Boolean = currentStateLock.synchronized {
    val state = clusterState
    val delta = state.upkeep
    state != (state `merge` delta)
  }

  def transformCluster(f: ClusterState => ClusterState): ClusterState = publish(
    f(currentStateLock.synchronized(clusterState))
  )

  def transformClient(f: ClientState => ClientState): ClientState = publishClient(
    f(currentStateLock.synchronized(clientState))
  )

  def handleIncoming(change: ClusterState): Unit = currentStateLock.synchronized {
    log(s"handling incoming $change")
    val (old, changed) = currentStateLock.synchronized {
      val old = clusterState
      clusterState = clusterState `merge` change
      (old, clusterState)
    }
    if old != changed then {
      val upkept = changed.upkeep
      if clusterState.subsumes(upkept)
      then log(s"no changes")
      else log(s"upkeep")
      assert(changed == clusterState)
      // else log(s"upkept: ${pprint(upkept)}")
      val newState = publish(upkept)
      maybeAnswerClient(old, newState)
      // try to propose a new value in case voting is decided
      maybeProposeNewValue(newState, clientState)
    }
  }

  private val kvCache = mutable.HashMap[String, String]()

  private def handleClientStateChange(change: ClientState): Unit = {
    log(s"handling incoming from client")
    val (old, changed) = currentStateLock.synchronized {
      val old = clientState
      clientState = clientState `merge` change
      (old, clientState)
    }
    if old != changed then {
      assert(changed == clientState)
      maybeProposeNewValue(clusterState, changed)
      // else log(s"upkept: ${pprint(upkept)}")
    }
  }

  private def maybeProposeNewValue(cluster: ClusterState, client: ClientState)(using LocalUid): Unit = {
    // check if we are the leader and ready to handle a request
    if cluster.leader.contains(replicaId) && cluster.phase == MultipaxosPhase.Idle then
      // ready to propose value
      client.firstUnansweredRequest match
        case Some(req) =>
          log(s"Proposing new value $req.")
          val _ = transformCluster(_.proposeIfLeader(req))
        case None =>
          log("I am the leader but request queue is empty.")
  }

  private def maybeAnswerClient(oldState: ClusterState, newState: ClusterState): Unit = {
    log(s"log: ${newState.log}")
    // println(s"${pprint.tokenize(newState).mkString("")}")

    for req @ Req(op, _, _) <- newState.readDecisionsSince(oldState.rounds.counter) do {
      val decision: String = op match {
        case KVOperation.Read(key) =>
          kvCache.synchronized {
            kvCache.getOrElse(key, s"Key '$key' has not been written to!")
          }
        case KVOperation.Write(key, value) =>
          kvCache.synchronized {
            kvCache.put(key, value)
          }
          s"$key=$value; OK"
      }
      transformClient { _.respond(req, decision) }
    }

  }
}
