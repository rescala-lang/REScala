package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv.tasks.TaskBundle
import rescala.fullmv.{
  FullMVBundle, FullMVUtil, FullMvStateBundle, TransactionSpanningTreeNode, TurnImplBundle, TurnPhase
}

import scala.concurrent.Future
import scala.concurrent.duration.Duration

trait FullMVTurnLocalCloneBundle extends FullMVBundle {
  selfType: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle
    with FullMVTurnReflectionBundle =>

  object FullMVTurnLocalClone {
    def withPredecessorReplication(
        turn: FullMVTurn,
        reflectionHost: FullMVEngine,
        fakeDelay: Duration = Duration.Zero
    ): FullMVTurn = {
      val clone            = FullMVTurnLocalClone(turn, reflectionHost, fakeDelay)
      val startReplication = turn.clockedPredecessors
      clone.ensurePredecessorReplication(
        startReplication._1.map(FullMVTurnLocalClone(_, reflectionHost, fakeDelay)),
        startReplication._2
      )
      clone
    }
    def apply(turn: FullMVTurn, reflectionHost: FullMVEngine, fakeDelay: Duration = Duration.Zero): FullMVTurn = {
      val phase = turn.phase
      assert(phase > TurnPhase.Uninitialized, s"trying to clone uninitialized turn")
      val active = phase < TurnPhase.Completed
      if (active) {
        reflectionHost.getCachedOrReceiveRemote(
          turn.guid, {
            val mirrorHost                   = turn.host
            val localMirror: FullMVTurnProxy = turn
            val mirrorProxy: FullMVTurnProxy = new FullMVTurnProxy {
              override def acquireRemoteBranchIfPhaseAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type] =
                FakeDelayer.requestReply(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.acquireRemoteBranchIfPhaseAtMost(maxPhase)
                )
              override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean] =
                FakeDelayer.requestReply(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.addPredecessor(tree.map((turn: FullMVTurn) =>
                    FullMVTurnLocalClone(turn, mirrorHost, fakeDelay)
                  ))
                )
              override def maybeNewReachableSubtree(
                  attachBelow: FullMVTurn,
                  spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]
              ): Future[Unit] =
                FakeDelayer.requestReply(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.maybeNewReachableSubtree(
                    FullMVTurnLocalClone(attachBelow, mirrorHost, fakeDelay),
                    spanningSubTreeRoot.map((turn: FullMVTurn) => FullMVTurnLocalClone(turn, mirrorHost, fakeDelay))
                  )
                )

              override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit =
                FakeDelayer.async(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.asyncRemoteBranchComplete(forPhase)
                )
              override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] =
                FakeDelayer.requestReply(reflectionHost, mirrorHost, fakeDelay, localMirror.addRemoteBranch(forPhase))
              override def newSuccessor(successor: FullMVTurn): Future[Unit] =
                FakeDelayer.requestReply(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.newSuccessor(FullMVTurnLocalClone(successor, mirrorHost, fakeDelay))
                )

              override def getLockedRoot: Future[LockStateResult] =
                FakeDelayer.requestReply(reflectionHost, mirrorHost, fakeDelay, localMirror.getLockedRoot)
              override def remoteTryLock(): Future[TryLockResult] =
                FakeDelayer.requestReply(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.remoteTryLock().map {
                    case Locked(lockedRoot) =>
                      Locked(SubsumableLockLocalClone(lockedRoot, reflectionHost.lockHost, fakeDelay))
                    case Blocked     => Blocked
                    case Deallocated => Deallocated
                  }(FullMVUtil.notWorthToMoveToTaskpool)
                )
              override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] =
                FakeDelayer.requestReply(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.remoteTrySubsume(SubsumableLockLocalClone(
                    lockedNewParent,
                    mirrorHost.lockHost,
                    fakeDelay
                  ))
                )

              override def asyncAddPhaseReplicator(
                  replicator: FullMVTurnPhaseReflectionProxy,
                  knownPhase: TurnPhase.Type
              ): Unit =
                FakeDelayer.async(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.asyncAddPhaseReplicator(
                    new FullMVTurnPhaseReflectionProxy {
                      override def asyncNewPhase(phase: TurnPhase.Type): Unit =
                        FakeDelayer.async(mirrorHost, reflectionHost, fakeDelay, replicator.asyncNewPhase(phase))
                    },
                    knownPhase
                  )
                )
              override def asyncAddPredecessorReplicator(
                  replicator: FullMVTurnPredecessorReflectionProxy,
                  startAt: TransactionSpanningTreeNode[FullMVTurn],
                  clock: Int
              ): Unit =
                FakeDelayer.async(
                  reflectionHost,
                  mirrorHost,
                  fakeDelay,
                  localMirror.asyncAddPredecessorReplicator(
                    new FullMVTurnPredecessorReflectionProxy {
                      override def newPredecessors(
                          predecessors: TransactionSpanningTreeNode[FullMVTurn],
                          clock: Int
                      ): Future[Unit] =
                        FakeDelayer.requestReply(
                          mirrorHost,
                          reflectionHost,
                          fakeDelay,
                          replicator.newPredecessors(
                            predecessors.map((turn: FullMVTurn) =>
                              FullMVTurnLocalClone(turn, reflectionHost, fakeDelay)
                            ),
                            clock
                          )
                        )
                    },
                    startAt.map(FullMVTurnLocalClone(_, mirrorHost, fakeDelay)),
                    clock
                  )
                )
            }
            new FullMVTurnReflection(reflectionHost, turn.guid, phase, mirrorProxy)
          }
        ) match {
          case Instantiated(reflection) =>
            reflection.proxy.asyncAddPhaseReplicator(reflection, phase)
            if (FullMVUtil.DEBUG)
              println(s"[${Thread.currentThread().getName}] newly local-cloned $reflection from $turn")
            reflection
          case Found(reflection: FullMVTurnReflection) =>
            reflection.asyncNewPhase(phase)
            reflection
          case Found(notReflection) =>
            assert(
              phase <= notReflection.phase,
              s"apparently $notReflection has a newer phase ($phase) on a remote copy than the local original?"
            )
            notReflection
          case _ => throw IllegalStateException("should be unreachable, but compiler is unsure")
        }
      } else {
        new FullMVTurnReflection(reflectionHost, turn.guid, phase, null)
      }
    }
  }
}
