package rescala.fullmv

import java.util.concurrent.atomic.AtomicInteger

import rescala.engine.{EngineImpl, InitializationImpl, ValuePersistency}
import rescala.graph.{Pulsing, Reactive, Struct}
import rescala.fullmv.FramingBranchResult._
import rescala.fullmv.NotificationResultAction._
import NotificationOutAndSuccessorOperation._
import rescala.graph.ReevaluationResult.{Dynamic, Static}

import scala.util.Try

trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = NodeVersionHistory[P]
}

object FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  object sgt extends SerializationGraphTracking {
    var predecessors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())
    var successors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())

    override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn): OrderResult = synchronized {
      assert(defender.state > State.Initialized, "turns that were not started should never be involved in any operations")
      assert(contender.state > State.Initialized, "turns that were not started should never be involved in any operations")
      assert(contender.state < State.Completed, "a completed turn cannot be a contender.")
      if(defender.state == State.Completed) {
        FirstFirst
      } else if (predecessors(defender)(contender)) {
        SecondFirst
      } else if (predecessors(contender)(defender)) {
        FirstFirst
      } else if(defender.state < contender.state) {
        establishOrder(contender, defender)
        SecondFirst
      } else {
        establishOrder(defender, contender)
        FirstFirst
      }
    }

    private def establishOrder(first: FullMVTurn, second: FullMVTurn): Unit = {
      val allAfter = successors(second) + second
      val allBefore = predecessors(first) + first
      for(succ <- allAfter) predecessors += succ -> (predecessors(succ) ++ allBefore)
      for(pred <- allBefore) successors += pred -> (successors(pred) ++ allAfter)
    }

    override def getOrder(a: FullMVTurn, b: FullMVTurn): PartialOrderResult = synchronized {
      assert(a.state > State.Initialized, "turns that were not started should never be involved in any operations")
      assert(b.state > State.Initialized, "turns that were not started should never be involved in any operations")
      assert(a.state != State.Completed || b.state != State.Completed, "two completed turns are being compared.")
      if(a.state == State.Completed) {
        FirstFirst
      } else if (b.state == State.Completed) {
        SecondFirst
      } else if (predecessors(a)(b)) {
        SecondFirst
      } else if (predecessors(b)(a)) {
        FirstFirst
      } else {
        Unordered
      }
    }

    def discard(turn: FullMVTurn): Unit = synchronized {
      assert(turn.state == State.Completed, "Trying to discard an incomplete turn")
      assert(predecessors(turn).isEmpty, "Compelted turn still has predecessors")
      for(succ <- successors(turn)) predecessors += succ -> (predecessors(succ) - turn)
      successors -= turn
    }
  }

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn(sgt)
  override protected def executeInternal[R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: (FullMVTurn) => R): R = {
    // framing start
    turn.beginPhase(State.Framing, initialWrites.size)
    initialWrites.foreach(turn.incrementFrame)

    // framing completion
    // TODO this should be an await once we add in-turn parallelism
    assert(turn.activeBranches.get() == 0, s"${turn.activeBranches.get()} active branches remained after fullmv framing phase")
    awaitAllPredecessorsState(turn, State.Executing)

    // admission
    turn.beginPhase(State.Executing, initialWrites.size)
    val result = Try(admissionPhase(turn))

    // propagation start
    initialWrites.foreach(turn.notify(_, changed = result.isSuccess, None))

    // propagation completion
    awaitAllPredecessorsState(turn, State.Completed)
    // TODO this should be an await once we add in-turn parallelism
    assert(turn.activeBranches.get() == 0, s"${turn.activeBranches.get()} active branches remained after fullmv propagation phase")
    turn.beginPhase(State.Completed, -1)
    sgt.discard(turn)

    // result
    result.get
  }

  def awaitAllPredecessorsState(turn: FullMVTurn, atLeast: State.Type): Unit = {
    // Note that each turn on which this suspends has the opportunity to add additional predecessors to this turn
    // transitively. We do, however, not need to repeatedly lookup the set of all predecessors, thereby ignoring these,
    // because the turn on which this suspended will hold the suspension until these new transitive predecessors have
    // reached the same stage first. Thus, looking up our own predecessors again after a suspension might reveal
    // additional predecessors, but they would all have reached the required state already.
    sgt.predecessors(turn).foreach {
      _.awaitState(atLeast)
    }
  }
}

object State {
  type Type = Int
  val Initialized: Type = 0
  val Framing: Type = 1
  val Executing: Type = 2
  val Completed: Type = 3
}

class FullMVTurn(val sgt: SerializationGraphTracking) extends InitializationImpl[FullMVStruct] {
  lazy val preTurn = {
    val preTurn = new FullMVTurn(sgt)
    preTurn.beginPhase(State.Completed, -1)
    preTurn
  }
  /**
    * counts the sum of in-flight notifications, queued and in-progress reevaluations.
    */
  val activeBranches = new AtomicInteger()
  @volatile var state: State.Type = State.Initialized

  def beginPhase(state: State.Type, activeBranches: Int): Unit = synchronized {
    require(state > this.state, "Can only progress state forwards.")
    assert(this.activeBranches.get() == 0, s"cannot start phase $state because ${this.activeBranches.get()} branches are still active in phase ${this.state}!")
    this.activeBranches.set(activeBranches)
    this.state = state
    notifyAll()
  }

  def awaitState(atLeast: State.Type): Unit = synchronized {
    while(state < atLeast) {
      wait()
    }
  }

  def incrementFrame(node: Reactive[FullMVStruct]): Unit = {
    processBranching(node.state.incrementFrame(this))
  }

  def incrementSupersedeFrame(node: Reactive[FullMVStruct], superseded: FullMVTurn): Unit = {
    processBranching(node.state.incrementSupersedeFrame(this, superseded))
  }

  private def processBranching(result: FramingBranchResult): Unit = {
    result match {
      case FramingBranchEnd =>
        activeBranches.addAndGet(-1)
      case FramingBranchOut(out) =>
        if(out.size != 1) activeBranches.addAndGet(out.size - 1)
        out.foreach(incrementFrame)
      case FramingBranchOutSuperseding(out, supersede) =>
        if(out.size != 1) activeBranches.addAndGet(out.size - 1)
        out.foreach(d => incrementSupersedeFrame(d, supersede))
    }
  }

  def notify(node: Reactive[FullMVStruct], changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    val notificationResultAction = node.state.notify(this, changed, maybeFollowFrame)
    notificationResultAction match {
      case GlitchFreeReadyButQueued =>
        // in-flight notification turned into queued reevaluation = no branch count change
        // reevaluation is queued, so do nothing
      case ResolvedQueuedToUnchanged =>
        activeBranches.addAndGet(-1)
      case NotGlitchFreeReady =>
        activeBranches.addAndGet(-1)
      case GlitchFreeReady =>
        // no branch count change
        reevaluate(node)
      case out: NotificationOutAndSuccessorOperation =>
        processNotificationAndFollowOperation(node, changed = false, out)
    }
  }

  // TODO optimize mutual tail-recursion with reevaluate?
  private def processNotificationAndFollowOperation(node: Reactive[FullMVStruct], changed: Boolean, out: NotificationOutAndSuccessorOperation): Unit = {
    out match {
      case NoSuccessor(out) =>
        sendNotifications(out, changed, None)
      case FollowFraming(out, succTxn) =>
        sendNotifications(out, changed, Some(succTxn))
      case NextReevaluation(out, succTxn) =>
        sendNotifications(out, changed, Some(succTxn))
        succTxn.reevaluate(node)
    }
  }

  // TODO optimize mutual tail-recursion with processNotificationAndFollowOperation?
  def reevaluate(node: Reactive[FullMVStruct]): Unit = {
      val result = node.reevaluate(this)
      result match {
        case res @ Static(isChange, value) =>
          val out = node.state.reevOut(this, if(isChange) Some(value) else None, None)
          processNotificationAndFollowOperation(node, isChange, out)
        case res @ Dynamic(isChange, value, deps) =>
          val diff = res.depDiff(node.state.incomings)
          diff.removed.foreach { drop =>
            val (successorWrittenVersions, maybeFollowFrame) = drop.state.drop(this, node)
            val removedQueuedReevaluations = node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
            for(turn <- removedQueuedReevaluations) turn.activeBranches.addAndGet(-1)
          }
          // TODO after adding in-turn parallelism, nodes may be more complete here than they were during actual reevaluation, leading to missed gltiches
          val anyPendingDependency = diff.added.foldLeft(false) { (anyPendingDependency, discover) =>
            val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, node)
            val addedQueuedReevaluations = node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
            for(turn <- addedQueuedReevaluations) turn.activeBranches.addAndGet(1)
            anyPendingDependency || (maybeFollowFrame == Some(this))
          }
          if(!anyPendingDependency) {
            val out = node.state.reevOut(this, if (isChange) Some(value) else None, Some(deps))
            processNotificationAndFollowOperation(node, isChange, out)
          }
      }
    }

  private def sendNotifications(out: Set[Reactive[FullMVStruct]], changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    if(out.size != 1) activeBranches.addAndGet(out.size - 1)
    out.foreach(notify(_, changed, maybeFollowFrame))
  }

  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P] = {
    val state = new NodeVersionHistory(sgt, preTurn, valuePersistency)
    state.incrementFrame(this)
    state
  }
  override protected def ignite(reactive: Reactive[FullMVStruct], incoming: Set[Reactive[FullMVStruct]], valuePersistency: ValuePersistency[_]): Unit = {
    activeBranches.addAndGet(1)
    incoming.foreach { discover =>
      dynamicDependencyInteraction(discover)
      val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
      val addedQueuedReevaluations = reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
      for(turn <- addedQueuedReevaluations) turn.activeBranches.addAndGet(1)
    }
    reactive.state.incomings = incoming
    notify(reactive, changed = valuePersistency.ignitionRequiresReevaluation, None)
  }

  override private[rescala] def dynamicDependencyInteraction(reactive: Reactive[FullMVStruct]) = reactive.state.synchronizeDynamicAccess(this)
  override private[rescala] def before[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticBefore(this)
  override private[rescala] def after[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticNow(this)

  override def observe(f: () => Unit): Unit = f()
}
