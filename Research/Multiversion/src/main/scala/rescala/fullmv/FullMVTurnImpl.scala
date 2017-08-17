package rescala.fullmv

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.locks.{LockSupport, ReentrantLock}

import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.{FullMVTurnReflectionProxy, Host}
import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

class FullMVTurnImpl(override val host: FullMVEngine, override val guid: Host.GUID, val userlandThread: Thread, initialLock: SubsumableLock) extends FullMVTurn {
  // counts the sum of in-flight notifications, in-progress reevaluations.
  var activeBranches = new AtomicInteger(0)

  val phaseLock = new ReentrantLock()
  object phaseParking
  @volatile var phase: TurnPhase.Type = TurnPhase.Initialized

  val subsumableLock: AtomicReference[SubsumableLock] = new AtomicReference(initialLock)
  val successorsIncludingSelf: ArrayBuffer[FullMVTurn] = ArrayBuffer(this) // this is implicitly a set
  val selfNode = new TransactionSpanningTreeNode[FullMVTurn](this)
  @volatile var predecessorSpanningTreeNodes: Map[FullMVTurn, TransactionSpanningTreeNode[FullMVTurn]] = Map(this -> selfNode)

  var replicators: Set[FullMVTurnReflectionProxy] = Set.empty

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = activeBranchDifferential(forPhase, -1)

  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $forState")
    assert(differential != 0, s"$this received 0 branch diff")
    assert(activeBranches.get + differential >= 0, s"$this received branch diff into negative count")
    val remaining = activeBranches.addAndGet(differential)
    if(remaining == 0) {
      LockSupport.unpark(userlandThread)
    }
  }

  override def newBranchFromRemote(forState: TurnPhase.Type): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $forState")
    activeBranches.getAndIncrement()
  }

  //========================================================Local State Control============================================================

  def awaitAndSwitchPhase(newPhase: TurnPhase.Type): Unit = {
    assert(newPhase > this.phase, s"$this cannot progress backwards to phase $newPhase.")
    @tailrec def awaitAndAtomicCasPhaseAndGetReps(): Set[FullMVTurnReflectionProxy] = {
      awaitBranchCountZero()
      val compare = awaitAllPredecessorsPhase(newPhase)
      phaseLock.lock()
      val success = try {
        if (activeBranches.get == 0 && (predecessorSpanningTreeNodes eq compare)) {
          this.phase = newPhase
          phaseParking.synchronized {
            phaseParking.notifyAll()
          }
          if (newPhase == TurnPhase.Completed) {
            predecessorSpanningTreeNodes = Map.empty
            selfNode.children = Set.empty
            host.dropInstance(guid, this)
          }
          if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
          Some(replicators)
        } else {
          None
        }
      } finally {
        phaseLock.unlock()
      }
      success match {
        case None => awaitAndAtomicCasPhaseAndGetReps()
        case Some(x) => x
      }
    }
    val reps = awaitAndAtomicCasPhaseAndGetReps()
    val forwards = reps.map(_.newPhase(newPhase))
    for(call <- forwards) {
      Await.result(call, Duration.Zero) // TODO Duration.Inf
    }
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
  }


  private def awaitBranchCountZero(): Unit = {
    while (activeBranches.get > 0) {
      LockSupport.park(this)
    }
  }

  private def awaitAllPredecessorsPhase(atLeast: TurnPhase.Type) = {
    val preds = predecessorSpanningTreeNodes
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this awaiting phase $atLeast+ on predecessors $preds")
    preds.keySet.foreach { waitFor =>
      if(waitFor != this) waitFor.awaitPhase(atLeast)
    }
    preds
  }


  //========================================================Remote State Control============================================================

  override def awaitPhase(atLeast: TurnPhase.Type): Unit = phaseParking.synchronized {
    while(phase < atLeast) {
      phaseParking.wait()
    }
  }

  //========================================================Ordering Search and Establishment Interface============================================================

  def isTransitivePredecessor(txn: FullMVTurn): Boolean = {
    assert(txn.host == host, s"predecessor query for $txn before $this is hosted on ${txn.host} different from $host")
    predecessorSpanningTreeNodes.contains(txn)
  }


  override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = {
    // TODO think about how and where to try{}finally{unlock()} this..
    phaseLock.lock()
    Future.successful((phase, selfNode))
  }

  def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
    @inline def predecessor = predecessorSpanningTree.txn
    assert(predecessor.getLockedRoot.isDefined, s"establishing order $predecessor -> $this: predecessor not locked")
    assert(getLockedRoot.isDefined, s"establishing order $predecessor -> $this: successor not locked")
    assert(!isTransitivePredecessor(predecessor), s"attempted to establish already existing predecessor relation $predecessor -> $this")
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this new predecessor $predecessor.")
    val possiblyRemoteExecutions = successorsIncludingSelf.map(_.maybeNewReachableSubtree(this, predecessorSpanningTree))
    for(pre <- possiblyRemoteExecutions) {
      Await.result(pre, Duration.Zero) // TODO Duration.Inf
    }
    phaseLock.unlock()
  }

  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
    if (!isTransitivePredecessor(spanningSubTreeRoot.txn)) {
      val buffer = ArrayBuffer[FullMVTurn]()
      phaseLock.lock()
      val reps = try {
        copySubTreeRootAndAssessChildren(attachBelow, spanningSubTreeRoot, buffer)
        replicators
      } finally {
        phaseLock.unlock()
      }

      val newSuccessorRemoteCalls = buffer.map(_.newSuccessor(this))
      val newPredecessorRemoteCalls = reps.map(_.newPredecessors(buffer))

      for(call <- newSuccessorRemoteCalls) {
        Await.result(call, Duration.Zero) // TODO Duration.Inf
      }
      for(call <- newPredecessorRemoteCalls) {
        Await.result(call, Duration.Zero) // TODO Duration.Inf
      }
    }
    Future.successful(Unit)
  }

  private def copySubTreeRootAndAssessChildren(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn], buffer: collection.generic.Growable[FullMVTurn]): Unit = {
    val newTransitivePredecessor = spanningSubTreeRoot.txn
    assert(newTransitivePredecessor.host == host, s"new predecessor $newTransitivePredecessor of $this is hosted on ${newTransitivePredecessor.host} different from $host")
    buffer += newTransitivePredecessor
    val copiedSpanningTreeNode = new TransactionSpanningTreeNode(newTransitivePredecessor)
    predecessorSpanningTreeNodes += newTransitivePredecessor -> copiedSpanningTreeNode
    predecessorSpanningTreeNodes(attachBelow).children += copiedSpanningTreeNode

    for (child <- spanningSubTreeRoot.children) {
      if(!isTransitivePredecessor(child.txn)) {
        copySubTreeRootAndAssessChildren(newTransitivePredecessor, child, buffer)
      }
    }
  }

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
    assert(successor.host == host, s"new successor $successor of $this is hosted on ${successor.host} different from $host")
    successorsIncludingSelf += successor
    Future.successful(Unit)
  }

  override def asyncReleasePhaseLock(): Unit = phaseLock.unlock()

  //========================================================State Replication============================================================

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, Set[FullMVTurn]) = {
    phaseLock.lock()
    try{
      replicators += replicator
      (phase, predecessorSpanningTreeNodes.keySet - this)
    } finally {
      phaseLock.unlock()
    }
  }

  override def removeReplicator(replicator: FullMVTurnReflectionProxy): Unit = {
    phaseLock.lock()
    try{
      replicators -= replicator
    } finally {
      phaseLock.unlock()
    }
  }

  //========================================================SSG SCC Mutual Exclusion Control============================================================

  override def getLockedRoot: Option[Host.GUID] = subsumableLock.get.getLockedRoot
  override def lock(): SubsumableLock = {
    val l = subsumableLock.get()
    val res = l.lock()
    subsumableLock.compareAndSet(l, res)
    res
  }
  override def tryLock(): SubsumableLock.TryLockResult = {
    val l = subsumableLock.get()
    val res = l.tryLock()
    subsumableLock.compareAndSet(l, res.newParent)
    res
  }
  override def spinOnce(backoff: Long): SubsumableLock = {
    val l = subsumableLock.get()
    val res = l.spinOnce(backoff)
    subsumableLock.compareAndSet(l, res)
    res
  }
  override def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock] = {
    val l = subsumableLock.get()
    val res = l.trySubsume(lockedNewParent)
    subsumableLock.compareAndSet(l, res.getOrElse(lockedNewParent))
    res
  }
  override def subsume(lockedNewParent: SubsumableLock): Unit = {
    val l = subsumableLock.get()
    l.subsume(lockedNewParent)
    subsumableLock.compareAndSet(l, lockedNewParent)
  }
  override def unlock(): SubsumableLock = {
    val l = subsumableLock.get()
    val res = l.unlock()
    subsumableLock.compareAndSet(l, res)
    res
  }

  //========================================================ToString============================================================

  override def toString: String = synchronized {
    "FullMVTurn(" + guid + " on " + host + ", " + (phase match {
      case 0 => "Initialized"
      case 1 => "Framing("+activeBranches.get+")"
      case 2 => "Executing("+activeBranches.get+")"
      case 3 => "WrapUp"
      case 4 => "Completed"
    })+ ")"
  }
}
