package probench

import probench.Codecs.given
import probench.data.RequestResponseQueue.Req
import probench.data.*
import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.experiments.protocols.{MultiPaxos, MultipaxosPhase, Participants}
import replication.DeltaDissemination

import java.util.concurrent.{ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.ExecutionContext

trait State[T: Lattice] {

  val lock: AnyRef

  var state: T
  val dataManager: DeltaDissemination[T]

  def handleIncoming(delta: T): Unit
  def publish(delta: T): T
  def transform(f: T => T): T = publish(f(lock.synchronized(state)))

}

class KeyValueReplica(
    val uid: Uid,
    val votingReplicas: Set[Uid],
    offloadSending: Boolean = true
) {

  inline def log(inline msg: String): Unit =
    if false then println(s"[$uid] $msg")

  val sendingActor: ExecutionContext = {
    if offloadSending then
      val singleThreadExecutor: ExecutorService = Executors.newSingleThreadExecutor(r => {
        val thread = new Thread(r)
        thread.setDaemon(true)
        thread
      })

      ExecutionContext.fromExecutorService(singleThreadExecutor)
    else
      DeltaDissemination.executeImmediately
  }

  given Participants(votingReplicas)
  given localUid: LocalUid = LocalUid(uid)

  val currentStateLock: AnyRef = new {}

  private val kvCache = mutable.HashMap[String, String]()

  // ============== CLUSTER ==============

  val cluster: Cluster = new Cluster(currentStateLock, localUid, sendingActor)

  cluster.maybeLeaderElection()

  class Cluster(
      override val lock: AnyRef,
      localUid: LocalUid,
      sendingActor: ExecutionContext,
      var state: ClusterState = MultiPaxos.empty,
  ) extends State[ClusterState] {

    override val dataManager: DeltaDissemination[ClusterState] = DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = true,
      sendingActor = sendingActor
    )

    override def handleIncoming(delta: ClusterState): Unit = lock.synchronized {
      log(s"handling incoming $delta")
      val (old, changed) = lock.synchronized {
        val old = state
        state = state `merge` delta
        (old, state)
      }
      if old != changed then {
        val upkept = changed.upkeep
        if state.subsumes(upkept)
        then log(s"no changes")
        else log(s"upkeep")
        assert(changed == state)
        // else log(s"upkept: ${pprint(upkept)}")
        val newState = publish(upkept)
        maybeAnswerClient(old)
        // try to propose a new value in case voting is decided
        maybeProposeNewValue(client.state)
      }
    }

    override def publish(delta: ClusterState): ClusterState = lock.synchronized {
      if delta `inflates` state then {
        log(s"publishing")
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else log(s"skip")

      state
    }

    def forceUpkeep(): ClusterState = currentStateLock.synchronized {
      publish(state.upkeep)
    }

    def needsUpkeep(): Boolean = currentStateLock.synchronized {
      val curState = state
      val delta    = curState.upkeep
      curState != (curState `merge` delta)
    }

    /** propose myself as leader if I have the lowest id */
    def maybeLeaderElection(): Unit = {
      votingReplicas.minOption match
        case Some(id) if id == uid =>
          transform(_.startLeaderElection): Unit
        case _ => ()
    }

    def maybeProposeNewValue(client: ClientState)(using LocalUid): Unit = {
      // check if we are the leader and ready to handle a request
      if state.leader.contains(replicaId) && state.phase == MultipaxosPhase.Idle then
        // ready to propose value
        client.firstUnansweredRequest match
          case Some(req) =>
            log(s"Proposing new value $req.")
            val _ = transform(_.proposeIfLeader(req))
          case None =>
            log("I am the leader but request queue is empty.")
    }

    def maybeAnswerClient(oldState: ClusterState): Unit = {
      log(s"log: ${state.log}")
      // println(s"${pprint.tokenize(newState).mkString("")}")

      for req @ Req(op, _, _) <- state.readDecisionsSince(oldState.rounds.counter) do {
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
        client.transform {
          _.respond(req, decision)
        }
      }

    }

  }

  // ============== CLIENT ==============

  val client: Client = new Client(currentStateLock, localUid, sendingActor)

  class Client(
      override val lock: AnyRef,
      localUid: LocalUid,
      sendingActor: ExecutionContext,
      var state: ClientState = RequestResponseQueue.empty
  ) extends State[ClientState] {

    override val dataManager: DeltaDissemination[ClientState] = DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = true,
      sendingActor = sendingActor
    )

    override def handleIncoming(delta: ClientState): Unit = {
      log(s"handling incoming from client")
      val (old, changed) = currentStateLock.synchronized {
        val old = state
        state = state `merge` delta
        (old, state)
      }
      if old != changed then {
        assert(changed == state)
        cluster.maybeProposeNewValue(changed)
        // else log(s"upkept: ${pprint(upkept)}")
      }
    }

    override def publish(delta: ClientState): ClientState = currentStateLock.synchronized {
      if delta `inflates` state then {
        log(s"publishing")
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else
        log(s"skip")

      state
    }

  }

  // ============== CONN-INF ==============

  val connInf: ConnInf = new ConnInf(currentStateLock, localUid, sendingActor)

  class ConnInf(
      override val lock: AnyRef,
      localUid: LocalUid,
      sendingActor: ExecutionContext,
      var state: ConnInformation = Map.empty,
      val timeoutThreshold: Long = 5000
  ) extends State[ConnInformation] {

    override val dataManager: DeltaDissemination[ConnInformation] = DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = false,
      sendingActor = sendingActor
    )

    override def handleIncoming(delta: ConnInformation): Unit = {
      log(s"handling incoming conn inf")
      val (old, changed) = currentStateLock.synchronized {
        val old = state
        state = state `merge` delta
        (old, state)
      }
    }

    override def publish(delta: ConnInformation): ConnInformation = currentStateLock.synchronized {
      if delta `inflates` state then {
        log(s"publishing conn inf")
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else log("skip publishing conn inf")

      state
    }

    def sendHeartbeat(): ConnInformation = {
      currentStateLock.synchronized {
        publish(Map.from(List((localUid, LastWriterWins.now(System.currentTimeMillis())))))
      }
    }

    def checkLiveness(): Unit = {
      state
        .map((uid, llw) => (uid, llw.value))
        .filter((_, time) => time < (System.currentTimeMillis() - timeoutThreshold))
        .foreach((uid, _) => println(s"$uid timed out"))
    }

  }

}
