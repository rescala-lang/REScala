package reactives.fullmv.mirrors

import java.util.concurrent.ForkJoinPool.ManagedBlocker
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import reactives.fullmv._
import reactives.fullmv.TurnPhase.Type
import reactives.fullmv.sgt.synchronization._
import reactives.fullmv.tasks.TaskBundle

import scala.annotation.tailrec
import scala.concurrent.Future

trait FullMVTurnReflectionBundle extends FullMVBundle {
  self: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle =>

  class FullMVTurnReflection(
      override val host: FullMVEngine,
      override val guid: Host.GUID,
      initialPhase: TurnPhase.Type,
      val proxy: FullMVTurnProxy
  ) extends FullMVTurn
      with SubsumableLockEntryPoint
      with FullMVTurnPhaseReflectionProxy
      with FullMVTurnPredecessorReflectionProxy
      with ManagedBlocker {
    val _phase: AtomicInteger          = new AtomicInteger(initialPhase)
    override def phase: TurnPhase.Type = _phase.get

    val predecessorIndex: AtomicReference[(Set[FullMVTurn], TransactionSpanningTreeNode[FullMVTurn], Int)] =
      new AtomicReference(null)
    override def selfNode: TransactionSpanningTreeNode[FullMVTurn] = {
      val maybeInitialized = predecessorIndex.get
      if (maybeInitialized == null) null else maybeInitialized._2
    }
    override def isTransitivePredecessor(txn: FullMVTurn): Boolean =
      (txn == this) || {
        val maybeInitialized = predecessorIndex.get
        maybeInitialized != null && maybeInitialized._1(txn)
      }

    var localBranchCountBuffer = new AtomicInteger(0)

    @volatile var predecessorSubscribed: Int = -1
    object predecessorSubscriptionParking

    override def clockedPredecessors: (TransactionSpanningTreeNode[FullMVTurn], Type) = {
      val (_, tree, clock) = predecessorIndex.get
      (tree, clock)
    }

    override def ensurePredecessorReplication(startAt: TransactionSpanningTreeNode[FullMVTurn], clock: Type): Unit = {
      val before = predecessorIndex.get
      if (
        before == null && predecessorIndex.compareAndSet(
          null,
          (reactives.fullmv.mirrors.FullMVTurnReflection.buildIndex(startAt), startAt, clock)
        )
      ) {
        proxy.asyncAddPredecessorReplicator(this, startAt, clock)
      }
    }

    override def isReleasable: Boolean = predecessorSubscriptionParking.synchronized { predecessorSubscribed != 0 }
    override def block(): Boolean =
      predecessorSubscriptionParking.synchronized {
        isReleasable || { predecessorSubscriptionParking.wait(); isReleasable }
      }

    override def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
      assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
      assert(differential != 0, s"$this received 0 branch diff")
      assert(localBranchCountBuffer.get + differential >= 0, s"$this received branch diff into negative count")
      val before = localBranchCountBuffer.getAndAdd(differential)
      val after  = before + differential
      if (before == 0) {
        if (FullMVUtil.DEBUG)
          println(s"[${Thread.currentThread().getName}] $this reactivated locally, registering remote branch.")
        FullMVUtil.myAwait(proxy.addRemoteBranch(forState), host.timeout)
      } else if (after == 0) {
        if (FullMVUtil.DEBUG)
          println(s"[${Thread.currentThread().getName}] $this done locally, deregistering remote branch.")
        proxy.asyncRemoteBranchComplete(forState)
      }
    }

    override def newBranchFromRemote(forState: TurnPhase.Type): Unit = {
      assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
      if (localBranchCountBuffer.getAndIncrement() != 0) {
        if (FullMVUtil.DEBUG)
          println(
            s"[${Thread.currentThread().getName}] $this received remote branch but still active; deregistering immediately."
          )
        proxy.asyncRemoteBranchComplete(forState)
      } else {
        if (FullMVUtil.DEBUG) println(s"[${Thread.currentThread().getName}] $this (re-)activated by remote branch.")
      }
    }

    override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn], clock: Int): Future[Unit] = {
      //  override def asyncNewPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
      if (phase < TurnPhase.Completed) {
        val newPreds = reactives.fullmv.mirrors.FullMVTurnReflection.buildIndex(predecessors)

        @inline
        @tailrec def retryCommitWhileNewer(): Future[Unit] = {
          val before = predecessorIndex.get
          if (before == null) {
            assert(phase == TurnPhase.Completed, s"shouldn't happen otherwise?")
            Future.unit
          } else if (before._3 < clock) {
            if (!predecessorIndex.compareAndSet(before, (newPreds, predecessors, clock))) {
              retryCommitWhileNewer()
            } else {
              if (FullMVUtil.DEBUG)
                println(s"[${Thread.currentThread().getName}] $this updated predecessors: $newPreds.")
              FullMVUtil.broadcast(predecessorReplicators.get)(_.newPredecessors(predecessors, clock))
            }
          } else {
            Future.unit
          }
        }

        retryCommitWhileNewer()
      } else {
        Future.unit
      }
    }

    final override def asyncNewPhase(phase: TurnPhase.Type): Unit = {
      val currentPhase = _phase.get
      if (currentPhase < phase) {
        val preds = predecessorIndex.get
        if (preds != null) {
          val iterator = preds._2.iterator()
          while (iterator.hasNext) {
            val child = iterator.next().txn
            if (child.phase < phase) {
              assert(
                child.isInstanceOf[FullMVTurnReflection],
                s"$this predecessor $child has phase < ${TurnPhase.toString(phase)}, but is not a reflection (i.e., this cannot be caused by the async phase transition message being delayed)"
              )
              // this case may occur if this reflection receives a phase transition notification, but has another reflection as predecessor,
              // who's base turn did a required preceeding phase transition properly before this reflections' base, but the corresponding
              // remote notification is delayed, e.g., because it must pass through more hops.
              if (FullMVUtil.DEBUG)
                println(
                  s"[${Thread.currentThread().getName}] $this pre-empting phase transition of predecessor $child to $phase."
                )
              // we therefore can pre-empt the other reflection's message, which makes it so that phase transitions on this host appear
              // consistent with the transactions predecessor order.
              child.asInstanceOf[FullMVTurnPhaseReflectionProxy].asyncNewPhase(phase)
            }
          }
        }
        if (!_phase.compareAndSet(currentPhase, phase)) {
          asyncNewPhase(currentPhase)
        } else {
          phaseReplicators.get.foreach(_.asyncNewPhase(phase))
          if (phase == TurnPhase.Completed) {
            host.dropInstance(guid, this)
            phaseReplicators.set(null)
            predecessorReplicators.set(null)
            predecessorIndex.set(null)
          }
          wakeWaitersAfterPhaseSwitch(phase)
        }
      }
    }

    override def asyncRemoteBranchComplete(forPhase: Type): Unit         = proxy.asyncRemoteBranchComplete(forPhase)
    override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = proxy.addRemoteBranch(forPhase)

    override def acquireRemoteBranchIfPhaseAtMost(maxPhase: Type): Future[TurnPhase.Type] = {
      val localOptimistic = phase
      if (localOptimistic <= maxPhase) {
        proxy.acquireRemoteBranchIfPhaseAtMost(maxPhase).map { phase =>
          asyncNewPhase(phase)
          phase
        }(FullMVUtil.notWorthToMoveToTaskpool)
      } else {
        Future.successful(localOptimistic)
      }
    }
    override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean] = {
      if (tree.txn.phase == TurnPhase.Completed) {
        if (FullMVUtil.DEBUG)
          println(
            s"[${Thread.currentThread().getName}] $this aborting predecessor addition of known completed ${tree.txn}"
          )
        Future.successful(true)
      } else {
        proxy.addPredecessor(tree).map { wasNotAddedBecauseCompleted =>
          val predecessor = tree.txn
          if (wasNotAddedBecauseCompleted && predecessor.phase < TurnPhase.Completed) {
            assert(
              predecessor.isInstanceOf[FullMVTurnReflection],
              s"$this predecessor $predecessor is not completed despite the remote host saying so, but is not a reflection (i.e., this cannot be caused by the async phase transition message being delayed)"
            )
            // this case may occur if this reflection tries to add a predecessor relationship for an uncompleted parameter reflection,
            // but on this reflection's base instance's host, the parameter turn is already known completed. In this case, the predecessor
            // relation addition returns without actually broadcasting an according edge. If the caller thus queries for this
            // relation after having tried to add it, this query may fail.
            if (FullMVUtil.DEBUG)
              println(
                s"[${Thread.currentThread().getName}] $this pre-empting completion transition of predecessor $predecessor"
              )
            // to make this case work correctly, the base instance returns true if the parameter turn is known completed.
            // we then here pre-empt the not-yet arrived phase transition for the parameter turn.
            predecessor.asInstanceOf[FullMVTurnPhaseReflectionProxy].asyncNewPhase(TurnPhase.Completed)
          }
          wasNotAddedBecauseCompleted
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
    }
    override def maybeNewReachableSubtree(
        attachBelow: FullMVTurn,
        spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]
    ): Future[Unit] = proxy.maybeNewReachableSubtree(attachBelow, spanningSubTreeRoot)

    override def newSuccessor(successor: FullMVTurn): Future[Unit] = proxy.newSuccessor(successor)

    override def getLockedRoot: Future[LockStateResult] = {
      if (proxy == null) {
        assert(phase == TurnPhase.Completed, s"incomplete $this without proxy?")
        CompletedState.futured
      } else
        proxy.getLockedRoot.map { res =>
          if (res == CompletedState) {
            if (SubsumableLock.DEBUG)
              println(s"[${Thread.currentThread().getName}] $this lock state result pre-empting completion transition")
            asyncNewPhase(TurnPhase.Completed)
          }
          res
        }(FullMVUtil.notWorthToMoveToTaskpool)
    }
    override def tryLock(): Future[TryLockResult] = {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this sending tryLock request")
      proxy.remoteTryLock().map { res =>
        if (SubsumableLock.DEBUG)
          println(
            s"[${Thread.currentThread().getName}] $this received tryLock result $res (retaining remote transfer reference as thread reference)"
          )
        if (res == Deallocated) {
          if (SubsumableLock.DEBUG)
            println(s"[${Thread.currentThread().getName}] $this try lock result pre-empting completion transition")
          asyncNewPhase(TurnPhase.Completed)
        }
        res
      }(FullMVUtil.notWorthToMoveToTaskpool)
    }
    override def remoteTryLock(): Future[TryLockResult] = {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through tryLock request")
      proxy.remoteTryLock().map { res =>
        if (SubsumableLock.DEBUG)
          println(
            s"[${Thread.currentThread().getName}] $this passing through tryLock result $res (retaining remote transfer reference as thread reference)"
          )
        if (res == Deallocated) {
          if (SubsumableLock.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this remote try lock result pre-empting completion transition"
            )
          asyncNewPhase(TurnPhase.Completed)
        }
        res
      }(FullMVUtil.notWorthToMoveToTaskpool)
    }
    override def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
      if (SubsumableLock.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] $this sending trySubsume $lockedNewParent request, adding remote parameter transfer reference"
        )
      lockedNewParent.localAddRefs(1)
      proxy.remoteTrySubsume(lockedNewParent).map { res =>
        if (SubsumableLock.DEBUG)
          println(s"[${Thread.currentThread().getName}] $this received trySubsume $lockedNewParent result $res")
        if (res == Deallocated) {
          if (SubsumableLock.DEBUG)
            println(s"[${Thread.currentThread().getName}] $this try subsume result pre-empting completion transition")
          asyncNewPhase(TurnPhase.Completed)
        }
        res
      }(FullMVUtil.notWorthToMoveToTaskpool)
    }
    override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
      if (SubsumableLock.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] $this passing through trySubsume $lockedNewParent request (passing on remote parameter reference)"
        )
      proxy.remoteTrySubsume(lockedNewParent).map { res =>
        if (SubsumableLock.DEBUG)
          println(s"[${Thread.currentThread().getName}] $this passing through trySubsume $lockedNewParent result $res")
        if (res == Deallocated) {
          if (SubsumableLock.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this remote try subsume result pre-empting completion transition"
            )
          asyncNewPhase(TurnPhase.Completed)
        }
        res
      }(FullMVUtil.notWorthToMoveToTaskpool)
    }

    override def toString: String =
      s"FullMVTurnReflection($guid on $host, ${TurnPhase.toString(phase)}${
          if (localBranchCountBuffer.get != 0) s"(${localBranchCountBuffer.get})"
          else ""
        })"
  }
}

object FullMVTurnReflection {
  def buildIndex[T](tree: TransactionSpanningTreeNode[T], accumulator: Set[T] = Set.empty[T]): Set[T] = {
    var accu = accumulator + tree.txn
    val it   = tree.iterator()
    while (it.hasNext) accu = buildIndex(it.next, accu)
    accu
  }
}
