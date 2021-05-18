package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.{
  LockStateResult, SubsumableLock, SubsumableLockBundle, TryLockResult, TrySubsumeResult
}
import rescala.fullmv.tasks.TaskBundle
import rescala.fullmv.{FullMVBundle, FullMvStateBundle, TransactionSpanningTreeNode, TurnImplBundle, TurnPhase}

trait Mirror extends FullMVBundle {
  selfType: TurnImplBundle with TaskBundle with SubsumableLockBundle with FullMvStateBundle =>

  trait FullMVTurnHost extends Host[FullMVTurn] {
    val lockHost: SubsumableLockHost
  }

  import scala.concurrent.Future

  trait FullMVTurnPhaseReflectionProxy {
    def asyncNewPhase(phase: TurnPhase.Type): Unit
  }

  trait FullMVTurnPredecessorReflectionProxy {
    def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn], clock: Int): Future[Unit]
  }

  trait FullMVTurnProxy {
    def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit]
    def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

    def acquireRemoteBranchIfPhaseAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type]
    def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean]

    def maybeNewReachableSubtree(
        attachBelow: FullMVTurn,
        spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]
    ): Future[Unit]
    def newSuccessor(successor: FullMVTurn): Future[Unit]

    def getLockedRoot: Future[LockStateResult]
    // result has one thread reference counted
    def remoteTryLock(): Future[TryLockResult]
    // parameter has one thread reference counted, result has one thread reference counted
    def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult]

    def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy, knownPhase: TurnPhase.Type): Unit
    def asyncAddPredecessorReplicator(
        replicator: FullMVTurnPredecessorReflectionProxy,
        startAt: TransactionSpanningTreeNode[FullMVTurn],
        clock: Int
    ): Unit
  }

  trait ReactiveReflectionProxy[-P] {
    def asyncIncrementFrame(turn: FullMVTurn): Unit
    def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit
    def asyncResolvedUnchanged(turn: FullMVTurn): Unit
    def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit
    def asyncNewValue(turn: FullMVTurn, value: P): Unit
    def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit
  }

}
