package rescala.fullmv

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.locks.{LockSupport, ReentrantReadWriteLock}

import rescala.core.{InitialChange, ReSource}
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.{FullMVTurnReflectionProxy, Host}
import rescala.fullmv.sgt.synchronization._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}

class FullMVTurnImpl(override val host: FullMVEngine, override val guid: Host.GUID, val userlandThread: Thread, initialLock: SubsumableLock) extends FullMVTurn {
  var initialChanges: collection.Map[ReSource[FullMVStruct], InitialChange[FullMVStruct]] = _

  // counts the sum of in-flight notifications, in-progress reevaluations.
  var activeBranches = new AtomicInteger(0)

  object replicatorLock
  val phaseLock = new ReentrantReadWriteLock()
  @volatile var phase: TurnPhase.Type = TurnPhase.Uninitialized

  val subsumableLock: AtomicReference[SubsumableLock] = new AtomicReference(initialLock)
  val successorsIncludingSelf: ArrayBuffer[FullMVTurn] = ArrayBuffer(this) // this is implicitly a set
  @volatile var selfNode = new MutableTransactionSpanningTreeRoot[FullMVTurn](this) // this is also implicitly a set
  @volatile var predecessorSpanningTreeNodes: Map[FullMVTurn, IMutableTransactionSpanningTreeNode[FullMVTurn]] = Map(this -> selfNode)

  var replicators: Set[FullMVTurnReflectionProxy] = Set.empty

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = {
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this branch on some remote completed")
    activeBranchDifferential(forPhase, -1)
  }

  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
    assert(differential != 0, s"$this received 0 branch diff")
    assert(activeBranches.get + differential >= 0, s"$this received branch diff into negative count")
    val remaining = activeBranches.addAndGet(differential)
    if(remaining == 0) {
      LockSupport.unpark(userlandThread)
    }
  }

  override def newBranchFromRemote(forPhase: Type): Unit = {
    assert(phase == forPhase, s"$this received branch differential for wrong state ${TurnPhase.toString(forPhase)}")
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this new branch on remote is actually loop-back to local")
    // technically, move one remote branch to a local branch, but as we don't count these separately, currently doing nothing.
  }

  override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = {
    assert(phase == forPhase, s"$this received branch differential for wrong state ${TurnPhase.toString(forPhase)}")
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this new branch on some remote")
    activeBranches.getAndIncrement()
    Future.unit
  }

  //========================================================Local State Control============================================================

  def awaitAndSwitchPhase(newPhase: TurnPhase.Type): Unit = {
    assert(newPhase > this.phase, s"$this cannot progress backwards to phase $newPhase.")
    @inline @tailrec def awaitAndSwitchPhase0(firstUnknownPredecessorIndex: Int, parkAfter: Long, registeredForWaiting: FullMVTurn): Iterable[FullMVTurnReflectionProxy] = {
      if (activeBranches.get() > 0) {
        if (registeredForWaiting != null) {
          registeredForWaiting.waiters.remove(this.userlandThread)
//          parkRestart.add(head.asInstanceOf[ReevaluationResultHandling[ReSource[FullMVStruct]]].node.toString)
//        } else if (parkAfter > 0) {
//          spinRestart.add(head.asInstanceOf[ReevaluationResultHandling[ReSource[FullMVStruct]]].node.toString)
        }
        awaitBranchCountZero()
        awaitAndSwitchPhase0(firstUnknownPredecessorIndex, 0L, null)
      } else if (firstUnknownPredecessorIndex == selfNode.size) {
        assert(registeredForWaiting == null, s"$this is still registered on $registeredForWaiting as waiter despite having finished waiting for it")
        phaseLock.writeLock.lock()
        // make thread-safe sure that we haven't received any new predecessors that might
        // not be in the next phase yet. Only once that's sure we can also thread-safe sure
        // check that no predecessors pushed any tasks into our queue anymore. And only then
        // can we phase switch.
        if (firstUnknownPredecessorIndex == selfNode.size && activeBranches.get() == 0) {
          val reps = replicatorLock.synchronized {
            this.phase = newPhase
            replicators
          }
          phaseLock.writeLock.unlock()
          reps
        } else {
          phaseLock.writeLock.unlock()
          awaitAndSwitchPhase0(firstUnknownPredecessorIndex, 0L, null)
        }
      } else {
        val currentUnknownPredecessor = selfNode.children(firstUnknownPredecessorIndex).txn
        if(currentUnknownPredecessor.phase < newPhase) {
          if (registeredForWaiting != null) {
            if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this parking for $currentUnknownPredecessor.")
            LockSupport.park(currentUnknownPredecessor)
            if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this unparked.")
            awaitAndSwitchPhase0(firstUnknownPredecessorIndex, 0L, currentUnknownPredecessor)
          } else {
            val now = System.nanoTime()
            val parkTimeSet = parkAfter > 0L
            if(parkTimeSet && now > parkAfter) {
              currentUnknownPredecessor.waiters.put(this.userlandThread, newPhase)
              awaitAndSwitchPhase0(firstUnknownPredecessorIndex, 0L, currentUnknownPredecessor)
            } else {
              val end = now + FullMVTurnImpl.CONSTANT_BACKOFF
              do {
                Thread.`yield`()
              } while (System.nanoTime() < end)
              awaitAndSwitchPhase0(firstUnknownPredecessorIndex, if(parkTimeSet) parkAfter else now + FullMVTurnImpl.MAX_BACKOFF, null)
            }
          }
        } else {
          if (registeredForWaiting != null) {
            currentUnknownPredecessor.waiters.remove(this.userlandThread)
            //            parkSwitch += 1
            //          } else if (parkAfter > 0) {
            //            spinSwitch += 1
          }
          awaitAndSwitchPhase0(firstUnknownPredecessorIndex + 1, 0L, null)
        }
      }
    }
    val reps = awaitAndSwitchPhase0(0, 0L, null)

    val it = waiters.entrySet().iterator()
    while (it.hasNext) {
      val waiter = it.next()
      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] ${FullMVTurnImpl.this} phase switch unparking ${waiter.getKey.getName}.")
      if (waiter.getValue <= newPhase) LockSupport.unpark(waiter.getKey)
    }

    Await.result(FullMVEngine.broadcast(reps) { _.newPhase(newPhase) }, host.timeout)

    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
  }

  private def awaitBranchCountZero(): Unit = {
    while (activeBranches.get > 0) {
      LockSupport.park(this)
    }
  }

  private def beginPhase(phase: TurnPhase.Type): Unit = {
    assert(this.phase == TurnPhase.Uninitialized, s"$this already begun")
    assert(activeBranches.get() == 0, s"$this cannot begin $phase: ${activeBranches.get()} branches active!")
    assert(selfNode.size == 0, s"$this cannot begin $phase: already has predecessors!")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this begun.")
    this.phase = phase
  }

  def beginFraming(): Unit = beginPhase(TurnPhase.Framing)
  def beginExecuting(): Unit = beginPhase(TurnPhase.Executing)

  //  def resetStatistics() = {
  //    spinSwitch = 0
  //    parkSwitch = 0
  //    spinRestart.clear()
  //    parkRestart.clear()
  //  }

  def completeFraming(): Unit = {
    assert(this.phase == TurnPhase.Framing, s"$this cannot complete framing: Not in framing phase")
    //    resetStatistics()
    awaitAndSwitchPhase(TurnPhase.Executing)
    assert(phase == TurnPhase.Executing, s"wat? $this")
    //    FullMVTurn.framesync.synchronized {
    //      val maybeCount1 = FullMVTurn.spinSwitchStatsFraming.get(spinSwitch)
    //      FullMVTurn.spinSwitchStatsFraming.put(spinSwitch, if(maybeCount1 == null) 1L else maybeCount1 + 1L)
    //      val maybeCount2 = FullMVTurn.parkSwitchStatsFraming.get(parkSwitch)
    //      FullMVTurn.parkSwitchStatsFraming.put(parkSwitch, if(maybeCount2 == null) 1L else maybeCount2 + 1L)
    //      val it1 = spinRestart.iterator()
    //      while(it1.hasNext) {
    //        val key = it1.next()
    //        val maybeCount3 = FullMVTurn.spinRestartStatsFraming.get(key)
    //        FullMVTurn.spinRestartStatsFraming.put(key, if (maybeCount3 == null) 1L else maybeCount3 + 1L)
    //      }
    //      val it2 = parkRestart.iterator()
    //      while(it2.hasNext) {
    //        val key = it2.next()
    //        val maybeCount4 = FullMVTurn.parkRestartStatsFraming.get(key)
    //        FullMVTurn.parkRestartStatsFraming.put(key, if (maybeCount4 == null) 1L else maybeCount4 + 1L)
    //      }
    //    }
  }

  def completeExecuting(): Unit = {
    assert(this.phase == TurnPhase.Executing, s"$this cannot complete executing: Not in executing phase")
    //    resetStatistics()
    awaitAndSwitchPhase(TurnPhase.Completed)
    predecessorSpanningTreeNodes = Map.empty
    selfNode = null
    val l = subsumableLock.getAndSet(null)
    if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this deallocating, dropping reference on $l.")
    l.localSubRefs(1)
    host.dropInstance(guid, this)
    //    FullMVTurn.execsync.synchronized {
    //      val maybeCount1 = FullMVTurn.spinSwitchStatsExecuting.get(spinSwitch)
    //      FullMVTurn.spinSwitchStatsExecuting.put(spinSwitch, if(maybeCount1 == null) 1L else maybeCount1 + 1L)
    //      val maybeCount2 = FullMVTurn.parkSwitchStatsExecuting.get(parkSwitch)
    //      FullMVTurn.parkSwitchStatsExecuting.put(parkSwitch, if(maybeCount2 == null) 1L else maybeCount2 + 1L)
    //      val it1 = spinRestart.iterator()
    //      while(it1.hasNext) {
    //        val key = it1.next()
    //        val maybeCount3 = FullMVTurn.spinRestartStatsExecuting.get(key)
    //        FullMVTurn.spinRestartStatsExecuting.put(key, if (maybeCount3 == null) 1L else maybeCount3 + 1L)
    //      }
    //      val it2 = parkRestart.iterator()
    //      while(it2.hasNext) {
    //        val key = it2.next()
    //        val maybeCount4 = FullMVTurn.parkRestartStatsExecuting.get(key)
    //        FullMVTurn.parkRestartStatsExecuting.put(key, if (maybeCount4 == null) 1L else maybeCount4 + 1L)
    //      }
    //    }
  }

  //========================================================Ordering Search and Establishment Interface============================================================

  def isTransitivePredecessor(txn: FullMVTurn): Boolean = {
    assert(txn.host == host, s"predecessor query for $txn before $this is hosted on ${txn.host} different from $host")
    predecessorSpanningTreeNodes.contains(txn)
  }

  def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type] = Future.successful {
    val pOptimistic = phase
    if(pOptimistic > maxPhase) {
      pOptimistic
    } else {
      phaseLock.readLock().lock()
      val pSecure = phase
      if (pSecure > maxPhase) phaseLock.readLock().unlock()
      pSecure
    }
  }

  def addPredecessor(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
    assert(predecessorSpanningTree.txn.phase > TurnPhase.Uninitialized, s"$this addition of initializing predecessor ${predecessorSpanningTree.txn} should be impossible")
    assert(Await.result(getLockedRoot, host.timeout).isDefined, s"addPredecessor while own lock isn't held")
    assert(Await.result(getLockedRoot, host.timeout) == Await.result(predecessorSpanningTree.txn.getLockedRoot, host.timeout) || predecessorSpanningTree.txn.phase == TurnPhase.Completed, s"addPredecessor while $this under lock ${Await.result(getLockedRoot, host.timeout)} but ${predecessorSpanningTree.txn} under ${Await.result(predecessorSpanningTree.txn.getLockedRoot, host.timeout)}.")
    assert(!isTransitivePredecessor(predecessorSpanningTree.txn), s"attempted to establish already existing predecessor relation ${predecessorSpanningTree.txn} -> $this")
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this new predecessor ${predecessorSpanningTree.txn}.")

    FullMVEngine.broadcast(successorsIncludingSelf) { _.maybeNewReachableSubtree(this, predecessorSpanningTree) }
  }

  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
    if (!isTransitivePredecessor(spanningSubTreeRoot.txn)) {
      val incompleteCallsAccumulator = FullMVEngine.newAccumulator()

      val reps = replicatorLock.synchronized {
        // accumulate all changes offline and then batch-publish them in a single volatile write.
        // this prevents concurrent threads from seeing some of the newly established relations,
        // but not yet some transitive ones of those, which may violate several assertions
        // (although this doesn't actually break anything beyond these assertions)
        predecessorSpanningTreeNodes = copySubTreeRootAndAssessChildren(predecessorSpanningTreeNodes, attachBelow, spanningSubTreeRoot, incompleteCallsAccumulator)
        replicators
      }

      FullMVEngine.accumulateBroadcastFutures(incompleteCallsAccumulator, reps) { _.newPredecessors(selfNode) }
      FullMVEngine.condenseCallResults(incompleteCallsAccumulator)
    } else {
      Future.unit
    }
  }

  private def copySubTreeRootAndAssessChildren(bufferPredecessorSpanningTreeNodes: Map[FullMVTurn, IMutableTransactionSpanningTreeNode[FullMVTurn]], attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn], newSuccessorCallsAccumulator: FullMVEngine.CallAccumulator[Unit]): Map[FullMVTurn, IMutableTransactionSpanningTreeNode[FullMVTurn]] = {
    val newTransitivePredecessor = spanningSubTreeRoot.txn
    assert(newTransitivePredecessor.host == host, s"new predecessor $newTransitivePredecessor of $this is hosted on ${newTransitivePredecessor.host} different from $host")
    if(spanningSubTreeRoot.txn.phase < TurnPhase.Completed) {
      val newSuccessorCall = newTransitivePredecessor.newSuccessor(this)
      FullMVEngine.accumulateFuture(newSuccessorCallsAccumulator, newSuccessorCall)

      val copiedSpanningTreeNode = new MutableTransactionSpanningTreeNode(newTransitivePredecessor)
      var updatedBufferPredecessorSpanningTreeNodes = bufferPredecessorSpanningTreeNodes + (newTransitivePredecessor -> copiedSpanningTreeNode)
      updatedBufferPredecessorSpanningTreeNodes(attachBelow).addChild(copiedSpanningTreeNode)

      val it = spanningSubTreeRoot.iterator()
      while (it.hasNext) {
        val child = it.next()
        if (!isTransitivePredecessor(child.txn)) {
          updatedBufferPredecessorSpanningTreeNodes = copySubTreeRootAndAssessChildren(updatedBufferPredecessorSpanningTreeNodes, newTransitivePredecessor, child, newSuccessorCallsAccumulator)
        }
      }
      updatedBufferPredecessorSpanningTreeNodes
    } else {
      bufferPredecessorSpanningTreeNodes
    }
  }

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
    assert(successor.host == host, s"new successor $successor of $this is hosted on ${successor.host} different from $host")
    successorsIncludingSelf += successor
    Future.unit
  }

  override def asyncReleasePhaseLock(): Unit = phaseLock.readLock().unlock()

  //========================================================State Replication============================================================

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn]) = {
    replicatorLock.synchronized {
      replicators += replicator
      (phase, selfNode)
    }
  }

  //========================================================SSG SCC Mutual Exclusion Control============================================================

  override def getLockedRoot: Future[Option[Host.GUID]] = {
    val l = subsumableLock.get
    if(l == null) SubsumableLock.futureNone else l.getLockedRoot
  }
  override def tryLock(): Future[Option[SubsumableLock]] = {
    val l = subsumableLock.get()
    l.tryLock0(0).flatMap {
      case Locked(failedRefChanges, newRoot) =>
        casLockAndNotifyFailedRefChanges(l, failedRefChanges, newRoot)
        Future.successful(Some(newRoot))
      case Blocked(failedRefChanges, newRoot) =>
        casLockAndNotifyFailedRefChanges(l, failedRefChanges, newRoot)
        SubsumableLock.futureNone
      case GarbageCollected =>
        assert(subsumableLock.get() != null, s"this should not be possible while locking only on the contender")
        assert(subsumableLock.get() != l, s"$l lock attempt returned GC'd although it is still in use")
        tryLock()
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
    val l = subsumableLock.get()
    if(l == null) {
      Deallocated.futured
    } else {
      l.trySubsume0(0, lockedNewParent).flatMap {
        case Successful(failedRefChanges) =>
          casLockAndNotifyFailedRefChanges(l, failedRefChanges, lockedNewParent)
          Successful.futured
        case Blocked(failedRefChanges, newRoot) =>
          casLockAndNotifyFailedRefChanges(l, failedRefChanges, newRoot)
          Blocked.futured
        case GarbageCollected =>
          trySubsume(lockedNewParent)
      }(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  private def casLockAndNotifyFailedRefChanges(from: SubsumableLock, failedRefChanges: Int, newRoot: SubsumableLock): Unit = {
    val finalFailedRefChanges = failedRefChanges + (if (from == newRoot) {
      0
    } else if(subsumableLock.compareAndSet(from, newRoot)) {
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this parent cas $from to $newRoot succeeded, dropping ref")
      from.localSubRefs(1)
      0
    } else {
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this parent cas $from to $newRoot failed due to contention")
      1
    })
    if(finalFailedRefChanges != 0) {
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this correcting $finalFailedRefChanges failed ref changes to $newRoot")
      newRoot.localSubRefs(finalFailedRefChanges)
    }
  }

  //========================================================ToString============================================================

  override def toString: String = s"FullMVTurn($guid on $host, ${TurnPhase.toString(phase)}${if(activeBranches.get != 0) s"(${activeBranches.get})" else ""})"
}

object FullMVTurnImpl {
  val CONSTANT_BACKOFF = 7500L // 7.5µs
  val MAX_BACKOFF = 100000L // 100µs

  //  object framesync
  //  var spinSwitchStatsFraming = new java.util.HashMap[Int, java.lang.Long]()
  //  var parkSwitchStatsFraming = new java.util.HashMap[Int, java.lang.Long]()
  //  val spinRestartStatsFraming = new java.util.HashMap[String, java.lang.Long]()
  //  val parkRestartStatsFraming =  new java.util.HashMap[String, java.lang.Long]()
  //  object execsync
  //  var spinSwitchStatsExecuting = new java.util.HashMap[Int, java.lang.Long]()
  //  var parkSwitchStatsExecuting = new java.util.HashMap[Int, java.lang.Long]()
  //  val spinRestartStatsExecuting = new java.util.HashMap[String, java.lang.Long]()
  //  val parkRestartStatsExecuting = new java.util.HashMap[String, java.lang.Long]()
}
