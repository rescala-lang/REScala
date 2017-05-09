package rescala.fullmv

import java.util.concurrent.atomic.AtomicInteger

import rescala.engine.{EngineImpl, InitializationImpl, ValuePersistency}
import rescala.graph.{Pulsing, Reactive, Struct}
import rescala.fullmv.FramingBranchResult._
import rescala.fullmv.NotificationResultAction._
import NotificationOutAndSuccessorOperation._
import rescala.graph.ReevaluationResult.{Dynamic, Static}

import scala.annotation.tailrec

trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = NodeVersionHistory[P]
}

object FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  object sgt extends SerializationGraphTracking {
    var predecessors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())
    var successors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())

    override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn): OrderResult = synchronized {
      if (predecessors(defender)(contender)) {
        SecondFirst
      } else if (predecessors(contender)(defender)) {
        FirstFirst
      } else {
        val allAfter = successors(contender) + contender
        val allBefore = predecessors(defender) + defender
        for(succ <- allAfter) predecessors += succ -> (predecessors(succ) ++ allBefore)
        for(pred <- allBefore) successors += pred -> (successors(pred) ++ allAfter)
        FirstFirst
      }
    }
    override def getOrder(a: FullMVTurn, b: FullMVTurn): PartialOrderResult = synchronized {
      if (predecessors(a)(b)) {
        SecondFirst
      } else if (predecessors(b)(a)) {
        FirstFirst
      } else {
        Unordered
      }
    }
  }

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn(sgt)
  override protected def executeInternal[R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: (FullMVTurn) => R): R = {
    // framing
    turn.activeBranches.set(initialWrites.size)
    initialWrites.foreach(turn.incrementFrame(_))
    assert(turn.activeBranches.get() == 0, s"${turn.activeBranches.get()} active branches remained after fullmv framing phase")

    // admission
    val result = admissionPhase(turn)

    // propagation
    turn.activeBranches.set(initialWrites.size)
    initialWrites.foreach(turn.notify(_, changed = true, None))
    assert(turn.activeBranches.get() == 0, s"${turn.activeBranches.get()} active branches remained after fullmv propagation phase")

    // result
    result
  }
}

class FullMVTurn(val sgt: SerializationGraphTracking) extends InitializationImpl[FullMVStruct] {
  lazy val preTurn = {
    val preTurn = new FullMVTurn(sgt)
    assert(sgt.ensureOrder(preTurn, this) == FirstFirst, "preTurn could not be established")
    preTurn
  }
  val activeBranches = new AtomicInteger()

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
        activeBranches.addAndGet(out.size - 1)
        out.foreach(incrementFrame)
      case FramingBranchOutSuperseding(out, supersede) =>
        activeBranches.addAndGet(out.size - 1)
        out.foreach(d => incrementSupersedeFrame(d, supersede))
    }
  }

  def notify(node: Reactive[FullMVStruct], changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    val notificationResultAction = node.state.notify(this, changed, maybeFollowFrame)
    notificationResultAction match {
      case GlitchFreeReadyButQueued =>
        // no branch count change
        // do nothing
      case ResolvedQueuedToUnchanged =>
        activeBranches.addAndGet(-1)
      case NotGlitchFreeReady =>
        activeBranches.addAndGet(-1)
      case GlitchFreeReady =>
        // no branch count change
        reevaluate(node)
      case NoSuccessor(out) =>
        sendNotifications(out, changed = false, None)
      case FollowFraming(out, succTxn) =>
        sendNotifications(out, changed = false, Some(succTxn))
      case NextReevaluation(out, succTxn) =>
        sendNotifications(out, changed = false, Some(succTxn))
        succTxn.reevaluate(node)
    }
  }

  @tailrec
  final def reevaluate(node: Reactive[FullMVStruct]): Unit = {
      val result = node.reevaluate(this)
      val notificationOutAndSuccessorOperation = result match {
        case Static(isChange, value) =>
          node.state.reevOut(this, if(isChange) Some(value) else None, None)
        case res @ Dynamic(isChange, value, deps) =>
          val diff = res.depDiff(node.state.incomings)
          diff.removed.foreach { drop =>
            val (successorWrittenVersions, maybeFollowFrame) = drop.state.drop(this, node)
            node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
          }
          val anyPendingDependency = diff.added.foldLeft(false) { (anyPendingDependency, discover) =>
            val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, node)
            node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, +1)
            anyPendingDependency || (maybeFollowFrame == Some(this))
          }
          if(!anyPendingDependency) node.state.reevOut(this, if(isChange) Some(value) else None, Some(deps))
      }
      notificationOutAndSuccessorOperation match {
        case NoSuccessor(out) =>
          sendNotifications(out, result.isChange, None)
        case FollowFraming(out, succTxn) =>
          sendNotifications(out, result.isChange, Some(succTxn))
        case NextReevaluation(out, succTxn) =>
          sendNotifications(out, result.isChange, Some(succTxn))
          succTxn.reevaluate(node)
      }
    }

  private def sendNotifications(out: Set[Reactive[FullMVStruct]], changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    activeBranches.addAndGet(out.size - 1)
    out.foreach(notify(_, changed, maybeFollowFrame))
  }

  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P] = {
    val state = new NodeVersionHistory(sgt, preTurn, valuePersistency)
    activeBranches.addAndGet(1)
    state.incrementFrame(this)
    state
  }
  override protected def ignite(reactive: Reactive[FullMVStruct], incoming: Set[Reactive[FullMVStruct]], valuePersistency: ValuePersistency[_]): Unit = {
    incoming.foreach { discover =>
      dynamicDependencyInteraction(discover)
      val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
      reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, +1)
    }
    reactive.state.incomings = incoming
    activeBranches.addAndGet(1)
    notify(reactive, changed = valuePersistency.ignitionRequiresReevaluation, None)
  }

  override private[rescala] def dynamicDependencyInteraction(reactive: Reactive[FullMVStruct]) = reactive.state.synchronizeDynamicAccess(this)
  override private[rescala] def before[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticBefore(this)
  override private[rescala] def after[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticNow(this)

  override def observe(f: () => Unit): Unit = f()
}
