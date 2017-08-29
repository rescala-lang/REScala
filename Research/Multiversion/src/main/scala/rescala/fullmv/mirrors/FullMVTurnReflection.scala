package rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}
import rescala.fullmv.TurnPhase.Type

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class FullMVTurnReflection(override val host: FullMVEngine, override val guid: Host.GUID, override val proxy: FullMVTurnProxy, timeout: Duration) extends FullMVTurn(timeout) with SubsumableLockReflectionMethodsToProxy with FullMVTurnReflectionProxy {
  object phaseParking
  var phase: TurnPhase.Type = TurnPhase.Initialized
  object subLock
  @volatile var predecessors: Set[Host.GUID] = Set()

  var localBranchCountBuffer = new AtomicInteger(0)

  var replicators: Set[FullMVTurnReflectionProxy] = Set.empty

  override def awaitPhase(atLeast: TurnPhase.Type): Unit = phaseParking.synchronized {
    while(phase < atLeast) phaseParking.wait()
  }

  override def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
    assert(differential != 0, s"$this received 0 branch diff")
    assert(localBranchCountBuffer.get + differential >= 0, s"$this received branch diff into negative count")
    val before = localBranchCountBuffer.getAndAdd(differential)
    val after = before + differential
    if(before == 0) {
      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this reactivated locally, registering remote branch.")
      Await.result(proxy.addRemoteBranch(forState), timeout)
    } else if(after == 0) {
      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this done locally, deregistering remote branch.")
      proxy.asyncRemoteBranchComplete(forState)
    }
  }

  override def newBranchFromRemote(forState: TurnPhase.Type): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
    if(localBranchCountBuffer.getAndIncrement() != 0) {
      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this received remote branch but still active; deregistering immediately.")
      proxy.asyncRemoteBranchComplete(forState)
    } else {
      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this (re-)activated by remote branch.")
    }
  }

  override def isTransitivePredecessor(txn: FullMVTurn): Boolean = txn == this || predecessors(txn.guid)

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, Seq[Host.GUID]) = subLock.synchronized {
    replicators += replicator
    (phase, predecessors.toSeq)
  }

  override def newPredecessors(predecessors: Seq[Host.GUID]): Future[Unit] = {
    val reps = subLock.synchronized {
      this.predecessors ++= predecessors
      Future.successful(Unit)
      replicators
    }
    val forwards = reps.map(_.newPredecessors(predecessors))
    for(call <- forwards) {
      Await.result(call, timeout)
    }
    Future.successful(Unit)
  }

  override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
    val reps = subLock.synchronized {
      if(this.phase < phase) {
        this.phase = phase
        phaseParking.synchronized {
          phaseParking.notifyAll()
        }
        if (phase == TurnPhase.Completed) {
          predecessors = Set.empty
          host.dropInstance(guid, this)
        }
        replicators
      } else {
        Set.empty
      }
    }
    val forwards = reps.map(_.newPhase(phase))
    for (call <- forwards) {
      Await.result(call, timeout)
    }
    Future.successful(Unit)
  }

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = proxy.asyncRemoteBranchComplete(forPhase)
  override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = proxy.addRemoteBranch(forPhase)

  override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = proxy.acquirePhaseLockAndGetEstablishmentBundle()
  override def addPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.addPredecessorAndReleasePhaseLock(predecessorSpanningTree)
  override def asyncReleasePhaseLock(): Unit = proxy.asyncReleasePhaseLock()
  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.maybeNewReachableSubtree(attachBelow, spanningSubTreeRoot)

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = proxy.newSuccessor(successor)

  override def toString: String = s"FullMVTurnReflection($guid on $host, ${TurnPhase.toString(phase)}${if(localBranchCountBuffer.get != 0) s"(${localBranchCountBuffer.get})" else ""})"
}
