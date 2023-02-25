package rescala.fullmv

import rescala.core.{AdmissionTicket, Derived, Initializer, Observation, ReSource, ReadAs, SchedulerImpl, Transaction}
import rescala.fullmv.NotificationBranchResult.ReevOutBranchResult._
import rescala.fullmv.NotificationBranchResult._
import rescala.fullmv.mirrors.{Host, HostImpl, _}
import rescala.fullmv.sgt.synchronization.{SubsumableLockBundle, SubsumableLockEntryPoint}
import rescala.fullmv.tasks.TaskBundle

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{ConcurrentHashMap, ForkJoinPool, ForkJoinTask}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

trait FullMVBundle {
  selfType: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle =>

  type State[V] = FullMVState[V, FullMVTurn]

  type Reactive = ReSource.of[State]
  type OutDep   = Derived.of[State]

  trait FullMVState[V, T <: FullMVTurn] {
    val host: FullMVEngine

    var incomings: Set[Reactive] = Set.empty

    def latestValue: V

    /** entry point for regular framing
      *
      * @param txn the transaction visiting the node for framing
      */
    def incrementFrame(txn: T): FramingBranchResult[T, OutDep]

    /** entry point for superseding framing
      *
      * @param txn       the transaction visiting the node for framing
      * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
      */
    def incrementSupersedeFrame(txn: T, supersede: T): FramingBranchResult[T, OutDep]

    /** entry point for change/nochange notification reception
      *
      * @param txn     the transaction sending the notification
      * @param changed whether or not the dependency changed
      */
    def notify(txn: T, changed: Boolean): (Boolean, NotificationBranchResult[T, OutDep])

    /** entry point for change/nochange notification reception with follow-up framing
      *
      * @param txn         the transaction sending the notification
      * @param changed     whether or not the dependency changed
      * @param followFrame a transaction for which to create a subsequent frame, furthering its partial framing.
      */
    def notifyFollowFrame(txn: T, changed: Boolean, followFrame: T): (Boolean, NotificationBranchResult[T, OutDep])

    def reevIn(turn: T): V

    /** progress `firstFrame` forward until a `Version.isFrame` is encountered, and
      * return the resulting notification out (with reframing if subsequent write is found).
      */
    def reevOut(
        turn: T,
        maybeValue: Option[V],
        unchange: V => V
    ): NotificationBranchResult.ReevOutBranchResult[T, OutDep]

    /** entry point for before(this); may suspend.
      *
      * @param txn the executing transaction
      * @return the corresponding `Version.value` from before this transaction, i.e., ignoring the transaction's
      *         own writes.
      */
    def dynamicBefore(txn: T): V

    def staticBefore(txn: T): V

    /** entry point for after(this); may suspend.
      *
      * @param txn the executing transaction
      * @return the corresponding `Version.value` ` from after this transaction, i.e., awaiting and returning the
      *         transaction's own write if one has occurred or will occur.
      */
    def dynamicAfter(txn: T): V

    def staticAfter(txn: T): V

    /** entry point for discover(this, add). May suspend.
      *
      * @param txn the executing reevaluation's transaction
      * @param add the new edge's sink node
      * @return the appropriate [[rescala.fullmv. Version.value]].
      */
    def discover(txn: T, add: OutDep): (List[T], Option[T])

    /** entry point for drop(this, ticket.issuer); may suspend temporarily.
      *
      * @param txn    the executing reevaluation's transaction
      * @param remove the removed edge's sink node
      */
    def drop(txn: T, remove: OutDep): (List[T], Option[T])

    /** performs the reframings on the sink of a discover(n, this) with arity +1, or drop(n, this) with arity -1
      *
      * @param successorWrittenVersions the reframings to perform for successor written versions
      * @param maybeSuccessorFrame      maybe a reframing to perform for the first successor frame
      * @param arity                    +1 for discover adding frames, -1 for drop removing frames.
      */
    def retrofitSinkFrames(successorWrittenVersions: Seq[T], maybeSuccessorFrame: Option[T], arity: Int): Seq[T]
  }

  case class TransactionHandle(ti: FullMVTurn) extends Transaction[State] {
    override private[rescala] def access(reactive: ReSource.of[State]): reactive.Value = ti.dynamicBefore(reactive)
    override def initializer: Initializer[State]                                       = ti
    override def observe(obs: Observation): Unit                                       = ti.observe(() => obs.execute())
  }

  class FullMVEngine(val timeout: Duration, val schedulerName: String)
      extends SchedulerImpl[State, TransactionHandle]
      with FullMVTurnHost
      with HostImpl[FullMVTurn] {

    override object lockHost extends SubsumableLockHostImpl {
      override def toString: String = s"[LockHost ${hashCode()} for $schedulerName ${schedulerName.hashCode}]"
    }
    override val dummy: FullMVTurnImpl = {
      val dummy = new FullMVTurnImpl(this, Host.dummyGuid, null, lockHost.newLock())
      instances.put(Host.dummyGuid, dummy)
      dummy.beginExecuting()
      dummy.completeExecuting()
      if (Host.DEBUG || SubsumableLock.DEBUG || FullMVUtil.DEBUG)
        println(s"[${Thread.currentThread().getName}] $this SETUP COMPLETE")
      dummy
    }
    def newTurn(): FullMVTurnImpl = createLocal(new FullMVTurnImpl(this, _, Thread.currentThread(), lockHost.newLock()))

    val threadPool = new ForkJoinPool() with ExecutionContext {
      override def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
    }

    override private[rescala] def singleReadValueOnce[A](reactive: ReadAs.of[State, A]) =
      reactive.read(reactive.state.latestValue)

    override def forceNewTransaction[R](
        declaredWrites: Set[ReSource.of[State]],
        admissionPhase: (AdmissionTicket[State]) => R
    ): R = {
      val turn        = newTurn()
      val transaction = TransactionHandle(turn)
      withDynamicInitializer(transaction) {
        if (declaredWrites.nonEmpty) {
          // framing phase
          turn.beginFraming()
          turn.activeBranchDifferential(TurnPhase.Framing, declaredWrites.size)
          for (i <- declaredWrites) threadPool.submit(new Framing(turn, i))
          turn.completeFraming()
        } else {
          turn.beginExecuting()
        }

        // admission phase
        val admissionTicket = new AdmissionTicket[State](transaction, declaredWrites)
        val admissionResult = Try { admissionPhase(admissionTicket) }
        if (FullMVUtil.DEBUG) admissionResult match {
          case scala.util.Failure(e) => e.printStackTrace()
          case _                     =>
        }
        assert(turn.activeBranches.get == 0, s"Admission phase left ${turn.activeBranches.get()} tasks undone.")

        // propagation phase
        if (declaredWrites.nonEmpty) {
          turn.initialChanges = admissionTicket.initialChanges
          turn.activeBranchDifferential(TurnPhase.Executing, declaredWrites.size)
          for (write <- declaredWrites)
            threadPool.submit(new SourceNotification(
              turn,
              write,
              admissionResult.isSuccess && turn.initialChanges.contains(write)
            ))
        }

        // turn completion
        turn.completeExecuting()

        // wrap-up "phase"
        val transactionResult =
          if (admissionTicket.wrapUp == null) {
            admissionResult
          } else {
            admissionResult.map { i =>
              // executed in map call so that exceptions in wrapUp make the transaction result a Failure
              admissionTicket.wrapUp(transaction)
              i
            }
          }

        // result
        transactionResult.get
      }
    }

    override def toString: String = s"[Engine $schedulerName ${hashCode()}]"
    def cacheStatus: String = s"${instances.size()} turn instances and ${lockHost.instances.size()} lock instances"
  }

  trait FullMVTurn
      extends Initializer[State]
      with FullMVTurnProxy
      with SubsumableLockEntryPoint
      with Hosted[FullMVTurn] {
    override val host: FullMVEngine

    // ========================================================Internal Management============================================================

    // ===== Turn State Manangement External API
// TODO draft for async turn phase transitions
//  val executingWaiters = AtomicReference[List[() => Unit]]
//  val completionWaiters = AtomicReference[List[() => Unit]]
    val waiters = new ConcurrentHashMap[Thread, TurnPhase.Type]()
    def wakeWaitersAfterPhaseSwitch(newPhase: TurnPhase.Type): Unit = {
      val it = waiters.entrySet().iterator()
      while (it.hasNext) {
        val waiter = it.next()
        if (FullMVUtil.DEBUG)
          println(s"[${Thread.currentThread().getName}] $this phase switch unparking ${waiter.getKey.getName}.")
        if (waiter.getValue <= newPhase) LockSupport.unpark(waiter.getKey)
      }
    }

    def selfNode: TransactionSpanningTreeNode[FullMVTurn]
    // should be mirrored/buffered locally
    def phase: TurnPhase.Type // must implement a read barrier
    def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit
    def newBranchFromRemote(forState: TurnPhase.Type): Unit

    // ===== Ordering Search&Establishment External API
    // should be mirrored/buffered locally
    def isTransitivePredecessor(txn: FullMVTurn): Boolean

    // ========================================================Remote Replication============================================================

    val phaseReplicators: AtomicReference[List[FullMVTurnPhaseReflectionProxy]] =
      new AtomicReference(Nil) // implicit set, write accesses are synchronized through CAS
    override def asyncAddPhaseReplicator(
        replicator: FullMVTurnPhaseReflectionProxy,
        knownPhase: TurnPhase.Type
    ): Unit = {
      if (phase < TurnPhase.Completed) {
        val added = FullMVTurn.atomicAdd(phaseReplicators, replicator)
        assert(
          added || phase == TurnPhase.Completed,
          s"phase replicator addition should only return failure, if $this is completed"
        )
        if (knownPhase < phase) replicator.asyncNewPhase(phase)
      } else {
        replicator.asyncNewPhase(TurnPhase.Completed)
      }
    }

    def clockedPredecessors: (TransactionSpanningTreeNode[FullMVTurn], Int)
    val predecessorReplicators: AtomicReference[List[FullMVTurnPredecessorReflectionProxy]] =
      new AtomicReference(Nil) // implicit set, write accesses are synchronized through CAS

    override def asyncAddPredecessorReplicator(
        replicator: FullMVTurnPredecessorReflectionProxy,
        startAt: TransactionSpanningTreeNode[FullMVTurn],
        clock: Int
    ): Unit = {
      if (phase < TurnPhase.Completed) {
        val added = FullMVTurn.atomicAdd(predecessorReplicators, replicator)
        if (!added) {
          assert(
            phase == TurnPhase.Completed,
            s"phase replicator addition should only return failure, if $this is completed"
          )
        } else {
          ensurePredecessorReplication(startAt, clock)
          val (knownPreds, knownClock) = clockedPredecessors
          if (clock < knownClock) {
            replicator.newPredecessors(knownPreds, knownClock)
            ()
          }
        }
      }
    }

    final def ensurePredecessorReplication(clockedPredecessors: (TransactionSpanningTreeNode[FullMVTurn], Int)): Unit =
      ensurePredecessorReplication(clockedPredecessors._1, clockedPredecessors._2)
    def ensurePredecessorReplication(startAt: TransactionSpanningTreeNode[FullMVTurn], clock: Int): Unit

    // ========================================================Scheduler Interface============================================================

    override def makeDerivedStructState[V](initialValue: V): NonblockingSkipListVersionHistory[V, FullMVTurn] = {
      val state = new NonblockingSkipListVersionHistory[V, FullMVTurn](
        host.dummy,
        initialValue
      )
      state.incrementFrame(this)
      state
    }

    override protected def makeSourceStructState[V](initialValue: V)
        : NonblockingSkipListVersionHistory[V, FullMVTurn] = {
      val state = makeDerivedStructState(initialValue)
      val res   = state.notify(this, changed = false)
      assert(res == true -> PureNotifyOnly(Set.empty))
      state
    }

    override def initialize(
        reactive: Derived.of[State],
        incoming: Set[ReSource.of[State]],
        needsReevaluation: Boolean
    ): Unit = {
//    assert(Thread.currentThread() == userlandThread, s"$this ignition of $reactive on different thread ${Thread.currentThread().getName}")
      if (FullMVUtil.DEBUG) println(s"[${Thread.currentThread().getName}] $this igniting $reactive on $incoming")
      incoming.foreach { discover =>
        discover.state.dynamicAfter(this) // TODO should we get rid of this?
        val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
        reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1).foreach(
          _.activeBranchDifferential(TurnPhase.Executing, 1)
        )
      }
      reactive.state.incomings = incoming
      // Execute this notification manually to be able to execute a resulting reevaluation immediately.
      // Subsequent reevaluations from retrofitting will be added to the global pool, but not awaited.
      // This matches the required behavior where the code that creates this reactive is expecting the initial
      // reevaluation (if one is required) to have been completed, but cannot access values from subsequent turns
      // and hence does not need to wait for those.
      activeBranchDifferential(TurnPhase.Executing, 1)
      val ignitionNotification = new Notification(this, reactive, changed = needsReevaluation)
      ignitionNotification.deliverNotification() match {
        case (true, DoNothing) =>
          if (FullMVUtil.DEBUG)
            println(s"[${Thread.currentThread().getName}] $this initialize $reactive spawned a branch.")
        case (false, DoNothing) =>
          if (FullMVUtil.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this initialize $reactive did not spawn a branch or reevaluation."
            )
          activeBranchDifferential(TurnPhase.Executing, -1)
        case (retainBranch, ReevaluationReady) =>
          if (FullMVUtil.DEBUG)
            println(s"[${Thread.currentThread().getName}] $this initialize $reactive spawned reevaluation.")
          new Reevaluation(this, reactive).doReevaluation(retainBranch)
        case (true, NotifyAndReevaluationReadySuccessor(out, succTxn)) if out.isEmpty =>
          if (FullMVUtil.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this initialize $reactive spawned reevaluation for successor $succTxn."
            )
          activeBranchDifferential(TurnPhase.Executing, -1)
          val succReev = new Reevaluation(succTxn, reactive)
          if (ForkJoinTask.inForkJoinPool()) {
            succReev.fork()
            ()
          } else {
            host.threadPool.submit(succReev)
            ()
          }
        case (true, PureNotifyOnly(out)) if out.isEmpty =>
          activeBranchDifferential(TurnPhase.Executing, -1)
        case (true, NotifyAndNonReadySuccessor(out, _)) if out.isEmpty =>
          activeBranchDifferential(TurnPhase.Executing, -1)
        case other =>
          throw new AssertionError(s"$this initialize $reactive: unexpected result: $other")
      }
    }

    def discover(node: ReSource.of[State], addOutgoing: Derived.of[State]): Unit = {
      val /*r @*/ (successorWrittenVersions, maybeFollowFrame) = node.state.discover(this, addOutgoing)
//    assert((successorWrittenVersions ++ maybeFollowFrame).forall(retrofit => retrofit == this || retrofit.isTransitivePredecessor(this)), s"$this retrofitting contains predecessors: discover $node -> $addOutgoing retrofits $r from ${node.state}")
      if (FullMVUtil.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] Reevaluation($this,$addOutgoing) discovering $node -> $addOutgoing re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame"
        )
      val newBranches = addOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
      newBranches.foreach(_.activeBranchDifferential(TurnPhase.Executing, 1))
    }

    def drop(node: ReSource.of[State], removeOutgoing: Derived.of[State]): Unit = {
      val /*r @*/ (successorWrittenVersions, maybeFollowFrame) = node.state.drop(this, removeOutgoing)
//    assert((successorWrittenVersions ++ maybeFollowFrame).forall(retrofit => retrofit == this || retrofit.isTransitivePredecessor(this)), s"$this retrofitting contains predecessors: drop $node -> $removeOutgoing retrofits $r from ${node.state}")
      if (FullMVUtil.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] Reevaluation($this,$removeOutgoing) dropping $node -> $removeOutgoing de-queueing $successorWrittenVersions and de-framing $maybeFollowFrame"
        )
      val deletedBranches = removeOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
      deletedBranches.foreach(_.activeBranchDifferential(TurnPhase.Executing, -1))
    }

    private[rescala] def writeIndeps(node: Derived.of[State], indepsAfter: Set[ReSource.of[State]]): Unit =
      node.state.incomings = indepsAfter

    type State[V] = FullMVBundle.this.State[V]

    private[rescala] def staticBefore(reactive: ReSource.of[State])  = reactive.state.staticBefore(this)
    private[rescala] def staticAfter(reactive: ReSource.of[State])   = reactive.state.staticAfter(this)
    private[rescala] def dynamicBefore(reactive: ReSource.of[State]) = reactive.state.dynamicBefore(this)
    private[rescala] def dynamicAfter(reactive: ReSource.of[State])  = reactive.state.dynamicAfter(this)

    def observe(f: () => Unit): Unit = f()
  }

  object FullMVTurn {
    def atomicAdd[T](list: AtomicReference[List[T]], element: T): Boolean = {
      @tailrec def tryAdd(): Boolean = {
        val before = list.get()
        if (before != null) {
          if (!list.compareAndSet(before, element :: before)) {
            tryAdd()
          } else {
            true
          }
        } else {
          false
        }
      }
      tryAdd()
    }

  }

}
