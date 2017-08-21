package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FullMVTurnLocalClone {
  def apply(turn: FullMVTurn, reflectionHost: FullMVEngine): FullMVTurn = {
    reflectionHost.getCachedOrReceiveRemote(turn.guid) { cacheNow =>
      val mirrorHost = turn.host
      val localMirror: FullMVTurnProxy = turn
      val mirrorProxy: FullMVTurnProxy = new SubsumableLockLocalCloneProxy(mirrorHost.lockHost, localMirror, reflectionHost.lockHost) with FullMVTurnProxy {
        override def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
          localMirror.blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree.map(FullMVTurnLocalClone(_, mirrorHost)))
        }
        override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
          localMirror.maybeNewReachableSubtree(FullMVTurnLocalClone(attachBelow, mirrorHost), spanningSubTreeRoot.map(FullMVTurnLocalClone(_, mirrorHost)))
        }
        override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = {
          val (phase, spanningTree) = Await.result(localMirror.acquirePhaseLockAndGetEstablishmentBundle(), Duration.Zero)
          Future.successful(phase -> spanningTree.map(FullMVTurnLocalClone(_, reflectionHost)))
        }
        override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = localMirror.asyncRemoteBranchComplete(forPhase)
        override def newSuccessor(successor: FullMVTurn): Future[Unit] = localMirror.newSuccessor(FullMVTurnLocalClone(successor, mirrorHost))
        override def asyncReleasePhaseLock(): Unit = localMirror.asyncReleasePhaseLock()
      }

      val reflection = new FullMVTurnReflection(reflectionHost, turn.guid, mirrorProxy)
      cacheNow(reflection)

      val reflectionProxy = new FullMVTurnReflectionProxy {
        override def newPhase(phase: TurnPhase.Type): Future[Unit] = reflection.newPhase(phase)
        override def newPredecessors(predecessors: Iterable[Host.GUID]): Future[Unit] = reflection.newPredecessors(predecessors)
      }
      val (initPhase, initPreds) = turn.addReplicator(reflectionProxy)
      reflection.newPhase(initPhase)
      reflection.newPredecessors(initPreds)

      reflection
    }
  }
}
