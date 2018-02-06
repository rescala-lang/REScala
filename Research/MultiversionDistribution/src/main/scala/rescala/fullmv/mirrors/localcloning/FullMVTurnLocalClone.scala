package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

object FullMVTurnLocalClone {
  def active(turn: FullMVTurn, reflectionHost: FullMVEngine): FullMVTurn = {
    val instance = passive(turn, reflectionHost)
    instance.ensurePredecessorReplication()
    instance
  }

  def passive(turn: FullMVTurn, reflectionHost: FullMVEngine): FullMVTurn = {
    val phase = turn.phase
    assert(phase > TurnPhase.Uninitialized, s"trying to clone uninitialized turn")
    val active = phase < TurnPhase.Completed
    if(active) {
      reflectionHost.getCachedOrReceiveRemote(turn.guid, {
        val mirrorHost = turn.host
        val localMirror: FullMVTurnProxy = turn
        val mirrorProxy: FullMVTurnProxy = new FullMVTurnProxy {
          override def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type] = localMirror.acquirePhaseLockIfAtMost(maxPhase)
          override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = localMirror.addPredecessor(tree.map(FullMVTurnLocalClone.passive(_, mirrorHost)))
          override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = localMirror.maybeNewReachableSubtree(FullMVTurnLocalClone.passive(attachBelow, mirrorHost), spanningSubTreeRoot.map(FullMVTurnLocalClone.passive(_, mirrorHost)))

          override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = localMirror.asyncRemoteBranchComplete(forPhase)
          override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = localMirror.addRemoteBranch(forPhase)
          override def newSuccessor(successor: FullMVTurn): Future[Unit] = localMirror.newSuccessor(FullMVTurnLocalClone.passive(successor, mirrorHost))
          override def asyncReleasePhaseLock(): Unit = localMirror.asyncReleasePhaseLock()

          override def getLockedRoot: Future[Option[Host.GUID]] = localMirror.getLockedRoot
          override def remoteTryLock(): Future[TryLockResult] = localMirror.remoteTryLock().map {
            case Locked(lockedRoot) => Locked(SubsumableLockLocalClone(lockedRoot, reflectionHost.lockHost))
            case Blocked => Blocked
            case Deallocated => Deallocated
          } (FullMVEngine.notWorthToMoveToTaskpool)
          override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = localMirror.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost.lockHost))

          override def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy): Unit = localMirror.asyncAddPhaseReplicator(new FullMVTurnPhaseReflectionProxy {
            override def asyncNewPhase(phase: TurnPhase.Type): Unit = replicator.asyncNewPhase(phase)
          })
          override def addPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy): Future[TransactionSpanningTreeNode[FullMVTurn]] = localMirror.addPredecessorReplicator(new FullMVTurnPredecessorReflectionProxy {
            override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = replicator.newPredecessors(predecessors.map(FullMVTurnLocalClone.passive(_, reflectionHost)))
          }).map {
            _.map(FullMVTurnLocalClone.passive(_, reflectionHost))
          }(FullMVEngine.notWorthToMoveToTaskpool)
        }
        new FullMVTurnReflection(reflectionHost, turn.guid, phase, mirrorProxy)
      }) match {
        case Instantiated(reflection) =>
          reflection.proxy.asyncAddPhaseReplicator(reflection)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] newly local-cloned $reflection from $turn")
          reflection
        case Found(instance) => instance
      }
    } else {
      new FullMVTurnReflection(reflectionHost, turn.guid, phase, null)
    }
  }
}
