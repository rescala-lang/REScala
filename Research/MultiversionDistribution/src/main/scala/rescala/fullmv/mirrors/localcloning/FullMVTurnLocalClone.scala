package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

object FullMVTurnLocalClone {
  def apply(turn: FullMVTurn, reflectionHost: FullMVEngine): FullMVTurn = {
    val phase = turn.phase
    assert(phase > TurnPhase.Uninitialized, s"trying to clone uninitialized turn")
    val active = phase < TurnPhase.Completed
    if(active) {
      reflectionHost.getCachedOrReceiveRemote(turn.guid, {
        val mirrorHost = turn.host
        val localMirror: FullMVTurnProxy = turn
        val mirrorProxy: FullMVTurnProxy = new FullMVTurnProxy {
          override def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type] = localMirror.acquirePhaseLockIfAtMost(maxPhase)
          override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean] = localMirror.addPredecessor(tree.map(FullMVTurnLocalClone(_, mirrorHost)))
          override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = localMirror.maybeNewReachableSubtree(FullMVTurnLocalClone(attachBelow, mirrorHost), spanningSubTreeRoot.map(FullMVTurnLocalClone(_, mirrorHost)))

          override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = localMirror.asyncRemoteBranchComplete(forPhase)
          override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = localMirror.addRemoteBranch(forPhase)
          override def newSuccessor(successor: FullMVTurn): Future[Unit] = localMirror.newSuccessor(FullMVTurnLocalClone(successor, mirrorHost))
          override def asyncReleasePhaseLock(): Unit = localMirror.asyncReleasePhaseLock()

          override def getLockedRoot: Future[LockStateResult] = localMirror.getLockedRoot
          override def remoteTryLock(): Future[TryLockResult] = localMirror.remoteTryLock().map {
            case Locked(lockedRoot) => Locked(SubsumableLockLocalClone(lockedRoot, reflectionHost.lockHost))
            case Blocked => Blocked
            case Deallocated => Deallocated
          } (FullMVEngine.notWorthToMoveToTaskpool)
          override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = localMirror.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost.lockHost))

          override def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy, knownPhase: TurnPhase.Type): Unit = localMirror.asyncAddPhaseReplicator(new FullMVTurnPhaseReflectionProxy {
            override def asyncNewPhase(phase: TurnPhase.Type): Unit = replicator.asyncNewPhase(phase)
          }, knownPhase)
          override def addPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy): Future[TransactionSpanningTreeNode[FullMVTurn]] = localMirror.addPredecessorReplicator(new FullMVTurnPredecessorReflectionProxy {
            override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = replicator.newPredecessors(predecessors.map(FullMVTurnLocalClone(_, reflectionHost)))
          }).map {
            _.map(FullMVTurnLocalClone(_, reflectionHost))
          }(FullMVEngine.notWorthToMoveToTaskpool)
        }
        new FullMVTurnReflection(reflectionHost, turn.guid, phase, mirrorProxy)
      }) match {
        case Instantiated(reflection) =>
          reflection.proxy.asyncAddPhaseReplicator(reflection, phase)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] newly local-cloned $reflection from $turn")
          reflection
        case Found(reflection: FullMVTurnReflection) =>
          reflection.asyncNewPhase(phase)
          reflection
        case Found(notReflection) =>
          assert(phase <= notReflection.phase, s"apparently $notReflection has a newer phase ($phase) on a remote copy than the local original?")
          notReflection
      }
    } else {
      new FullMVTurnReflection(reflectionHost, turn.guid, phase, null)
    }
  }
}
