package rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}
import rescala.fullmv.TurnPhase.Type

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class FullMVTurnReflection(override val host: FullMVEngine, override val guid: Host.GUID, override val proxy: FullMVTurnProxy, initialPhase: TurnPhase.Type, initialPredecessors: Set[FullMVTurn]) extends FullMVTurn with SubsumableLockReflectionMethodsToProxy with FullMVTurnReflectionProxy {
  object phaseParking
  var phase: TurnPhase.Type = initialPhase
  object subLock
  @volatile var predecessors: Set[FullMVTurn] = initialPredecessors

  var localBranchCountBuffer = new AtomicInteger(0)

  var replicators: Set[FullMVTurnReflectionProxy] = Set.empty

  override def awaitPhase(atLeast: TurnPhase.Type): Unit = phaseParking.synchronized {
    while(phase < atLeast) phaseParking.wait()
  }

  override def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $forState")
    assert(differential != 0, s"$this received 0 branch diff")
    assert(localBranchCountBuffer.get + differential >= 0, s"$this received branch diff into negative count")
    val remaining = localBranchCountBuffer.addAndGet(differential)
    if(remaining == 0) {
      proxy.asyncRemoteBranchComplete(forState)
    }
  }

  override def newBranchFromRemote(forState: TurnPhase.Type): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $forState")
    if(localBranchCountBuffer.getAndIncrement() != 0) {
      proxy.asyncRemoteBranchComplete(forState)
    }
  }

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = activeBranchDifferential(forPhase, -1)

  override def isTransitivePredecessor(txn: FullMVTurn): Boolean = txn == this || predecessors(txn)

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, Set[FullMVTurn]) = subLock.synchronized {
    replicators += replicator
    (phase, predecessors)
  }

  override def removeReplicator(replicator: FullMVTurnReflectionProxy): Unit = subLock.synchronized {
    replicators -= replicator
  }

  override def newPredecessors(predecessors: Iterable[FullMVTurn]): Future[Unit] = {
    val reps = subLock.synchronized {
      this.predecessors ++= predecessors
      Future.successful(Unit)
      replicators
    }
    val forwards = reps.map(_.newPredecessors(predecessors))
    for(call <- forwards) {
      Await.result(call, Duration.Zero) // TODO Duration.Inf
    }
    Future.successful(Unit)
  }

  override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
    val reps = subLock.synchronized {
      this.phase = phase
      phaseParking.synchronized {
        phaseParking.notifyAll()
      }
      if(phase == TurnPhase.Completed) {
        predecessors = Set.empty
        host.dropInstance(guid, this)
      }
      replicators
    }
    val forwards = reps.map(_.newPhase(phase))
    for(call <- forwards) {
      Await.result(call, Duration.Zero) // TODO Duration.Inf
    }
    Future.successful(Unit)
  }

  override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = proxy.acquirePhaseLockAndGetEstablishmentBundle()
  override def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit = proxy.blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree)
  override def asyncReleasePhaseLock(): Unit = proxy.asyncReleasePhaseLock()
  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.maybeNewReachableSubtree(attachBelow, spanningSubTreeRoot)

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = proxy.newSuccessor(successor)

  override def toString: String = {
    "FullMVTurnReflection(" + guid + " on " + host + ", " + (phase match {
      case 0 => "Initialized"
      case 1 => "Framing("+localBranchCountBuffer.get+")"
      case 2 => "Executing("+localBranchCountBuffer.get+")"
      case 3 => "WrapUp"
      case 4 => "Completed"
    })+ ")"
  }
}
