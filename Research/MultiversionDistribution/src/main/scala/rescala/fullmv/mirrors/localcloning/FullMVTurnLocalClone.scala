package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

object FullMVTurnLocalClone {
  def apply(turn: FullMVTurn, reflectionHost: FullMVEngine): FullMVTurn = {
    reflectionHost.getCachedOrReceiveRemote(turn.guid, {
      val mirrorHost = turn.host
      val localMirror: FullMVTurnProxy = turn
      val mirrorProxy: FullMVTurnProxy = new FullMVTurnProxy {
        override def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type] = localMirror.acquirePhaseLockIfAtMost(maxPhase)
        override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = localMirror.addPredecessor(tree.map(FullMVTurnLocalClone(_, mirrorHost)))
        override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = localMirror.maybeNewReachableSubtree(FullMVTurnLocalClone(attachBelow, mirrorHost), spanningSubTreeRoot.map(FullMVTurnLocalClone(_, mirrorHost)))

        override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = localMirror.asyncRemoteBranchComplete(forPhase)
        override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = localMirror.addRemoteBranch(forPhase)
        override def newSuccessor(successor: FullMVTurn): Future[Unit] = localMirror.newSuccessor(FullMVTurnLocalClone(successor, mirrorHost))
        override def asyncReleasePhaseLock(): Unit = localMirror.asyncReleasePhaseLock()

        override def getLockedRoot: Future[Option[Host.GUID]] = localMirror.getLockedRoot
        override def remoteTryLock(): Future[TryLockResult] = localMirror.remoteTryLock().map {
          case Locked(lockedRoot) => Locked(SubsumableLockLocalClone(lockedRoot, reflectionHost.lockHost))
          case Blocked => Blocked
          case Deallocated => Deallocated
        } (FullMVEngine.notWorthToMoveToTaskpool)
        override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = localMirror.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost.lockHost))
      }

      val predTree = turn.selfNode
      val phase = turn.phase
      assert(predTree != null || phase == TurnPhase.Completed, s"predecessor tree of $turn was null, but turn still didn't show as completed")

      val reflection = new FullMVTurnReflection(reflectionHost, turn.guid, turn.phase, mirrorProxy)
      val active = phase < TurnPhase.Completed
      if(active) {
        // TODO this probably doesnt work due to endless loop; the predecessor turns have to subscibe asynchronously, not as a nested call.
        if(predTree != null) reflection.newPredecessors(predTree.map{ pred =>
          if(pred == reflection) reflection else FullMVTurnLocalClone(pred, reflectionHost)
        })

        val reflectionProxy = new FullMVTurnReflectionProxy {
          override def newPhase(phase: TurnPhase.Type): Future[Unit] = reflection.newPhase(phase)
          override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = reflection.newPredecessors(predecessors.map(FullMVTurnLocalClone(_, reflectionHost)))
        }
        val (initPhase, initPreds) = turn.addReplicator(reflectionProxy)
        reflection.newPhase(initPhase)
        reflection.newPredecessors(initPreds)
      }
      (active, reflection)
    }).instance
  }
}
