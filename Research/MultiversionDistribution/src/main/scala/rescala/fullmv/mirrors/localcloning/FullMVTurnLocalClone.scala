package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future
import scala.concurrent.duration.Duration

object FullMVTurnLocalClone {
  def apply(turn: FullMVTurn, reflectionHost: FullMVEngine, fakeDelay: Duration = Duration.Zero): FullMVTurn = {
    val phase = turn.phase
    assert(phase > TurnPhase.Uninitialized, s"trying to clone uninitialized turn")
    val active = phase < TurnPhase.Completed
    if(active) {
      reflectionHost.getCachedOrReceiveRemote(turn.guid, {
        val mirrorHost = turn.host
        val localMirror: FullMVTurnProxy = turn
        val mirrorProxy: FullMVTurnProxy = new FullMVTurnProxy {
          override def acquireRemoteBranchIfPhaseAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type] = FakeDelayer.future(fakeDelay, localMirror.acquireRemoteBranchIfPhaseAtMost(maxPhase))
          override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean] = FakeDelayer.future(fakeDelay, localMirror.addPredecessor(tree.map((turn: FullMVTurn) => FullMVTurnLocalClone(turn, mirrorHost, fakeDelay))))
          override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = FakeDelayer.future(fakeDelay, localMirror.maybeNewReachableSubtree(FullMVTurnLocalClone(attachBelow, mirrorHost, fakeDelay), spanningSubTreeRoot.map((turn: FullMVTurn) => FullMVTurnLocalClone(turn, mirrorHost, fakeDelay))))

          override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = FakeDelayer.async(fakeDelay, localMirror.asyncRemoteBranchComplete(forPhase))
          override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = FakeDelayer.future(fakeDelay, localMirror.addRemoteBranch(forPhase))
          override def newSuccessor(successor: FullMVTurn): Future[Unit] = FakeDelayer.future(fakeDelay, localMirror.newSuccessor(FullMVTurnLocalClone(successor, mirrorHost, fakeDelay)))

          override def getLockedRoot: Future[LockStateResult] = FakeDelayer.future(fakeDelay, localMirror.getLockedRoot)
          override def remoteTryLock(): Future[TryLockResult] = FakeDelayer.future(fakeDelay, localMirror.remoteTryLock().map {
            case Locked(lockedRoot) => Locked(SubsumableLockLocalClone(lockedRoot, reflectionHost.lockHost, fakeDelay))
            case Blocked => Blocked
            case Deallocated => Deallocated
          } (FullMVEngine.notWorthToMoveToTaskpool))
          override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = FakeDelayer.future(fakeDelay, localMirror.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost.lockHost, fakeDelay)))

          override def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy, knownPhase: TurnPhase.Type): Unit = FakeDelayer.async(fakeDelay, localMirror.asyncAddPhaseReplicator(new FullMVTurnPhaseReflectionProxy {
            override def asyncNewPhase(phase: TurnPhase.Type): Unit = FakeDelayer.async(fakeDelay, replicator.asyncNewPhase(phase))
          }, knownPhase))
          override def addPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy): Future[TransactionSpanningTreeNode[FullMVTurn]] = FakeDelayer.future(fakeDelay, localMirror.addPredecessorReplicator(new FullMVTurnPredecessorReflectionProxy {
            override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = FakeDelayer.future(fakeDelay, replicator.newPredecessors(predecessors.map((turn: FullMVTurn) => FullMVTurnLocalClone(turn, reflectionHost, fakeDelay))))
          }).map {
            _.map((turn: FullMVTurn) => FullMVTurnLocalClone(turn, reflectionHost, fakeDelay))
          }(FullMVEngine.notWorthToMoveToTaskpool))
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
