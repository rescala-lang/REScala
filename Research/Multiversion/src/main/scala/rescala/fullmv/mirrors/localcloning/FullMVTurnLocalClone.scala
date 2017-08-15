package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Success

object FullMVTurnLocalClone {
  def localCloneSpanningTree(node: TransactionSpanningTreeNode[FullMVTurn], reflectionHost: FullMVEngine): TransactionSpanningTreeNode[FullMVTurn] = {
    val clone = new TransactionSpanningTreeNode(FullMVTurnLocalClone(node.txn, reflectionHost))
    clone.children = node.children.map(localCloneSpanningTree(_, reflectionHost))
    clone
  }

  def apply(turn: FullMVTurn, reflectionHost: FullMVEngine): FullMVTurn = {
    reflectionHost.getCachedOrReceiveRemote(turn.guid) { cacheNow =>
      val mirrorHost = turn.host
      val localMirror: FullMVTurnMirrorProxy = turn
      val mirrorProxy: FullMVTurnMirrorProxy = new FullMVTurnMirrorProxy {
        override def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
          localMirror.blockingAddPredecessorAndReleasePhaseLock(localCloneSpanningTree(predecessorSpanningTree, mirrorHost))
        }
        override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
          localMirror.maybeNewReachableSubtree(FullMVTurnLocalClone(attachBelow, mirrorHost), localCloneSpanningTree(spanningSubTreeRoot, mirrorHost))
        }
        override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(Type, TransactionSpanningTreeNode[FullMVTurn])] = {
          val (phase, spanningTree) = Await.result(localMirror.acquirePhaseLockAndGetEstablishmentBundle(), Duration.Zero)
          Future.successful(phase -> localCloneSpanningTree(spanningTree, reflectionHost))
        }
        override def asyncRemoteBranchComplete(forPhase: Type): Unit = localMirror.asyncRemoteBranchComplete(forPhase)
        override def newSuccessor(successor: FullMVTurn): Future[Unit] = localMirror.newSuccessor(FullMVTurnLocalClone(successor, mirrorHost))
        override def asyncReleasePhaseLock(): Unit = localMirror.asyncReleasePhaseLock()
        override def spinOnce(backoff: Long): SubsumableLock.TryLockResult = SubsumableLockLocalClone.localCloneTryLockResult(localMirror.spinOnce(backoff), reflectionHost.lockHost)
        override def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock] = {
          localMirror.trySubsume(SubsumableLockLocalClone.localCloneTryLockResult(lockedNewParent, mirrorHost.lockHost)).map(SubsumableLockLocalClone(_, reflectionHost.lockHost))
        }
        override def tryLock(): SubsumableLock.TryLockResult = SubsumableLockLocalClone.localCloneTryLockResult(localMirror.tryLock(), reflectionHost.lockHost)
        override def lock(): SubsumableLock.TryLockResult = SubsumableLockLocalClone.localCloneTryLockResult(localMirror.lock(), reflectionHost.lockHost)
        override def getLockedRoot: Option[Host.GUID] = localMirror.getLockedRoot
      }

      val reflectionPromise = Promise[FullMVTurnReflection]
      val reflectionProxy: FullMVTurnReflectionProxy = new FullMVTurnReflectionProxy {
        override def newPhase(phase: Type): Future[Unit] = Await.result(reflectionPromise.future, Duration.Inf).newPhase(phase)
        override def newPredecessors(predecessors: Iterable[FullMVTurn]): Future[Unit] = Await.result(reflectionPromise.future, Duration.Inf).newPredecessors(predecessors.map(FullMVTurnLocalClone.apply(_, reflectionHost)))
      }
      val (initPhase, mirrorInitPreds) = turn.addReplicator(reflectionProxy)
      val reflectionInitPreds = mirrorInitPreds.map(FullMVTurnLocalClone(_, reflectionHost))

      val reflection = new FullMVTurnReflection(reflectionHost, turn.guid, mirrorProxy, initPhase, reflectionInitPreds)
      cacheNow(reflection)
      if(initPhase == TurnPhase.Completed) {
        reflectionHost.dropInstance(turn.guid, reflection)
      }
      reflectionPromise.complete(Success(reflection))
      reflection
    }
  }
}
