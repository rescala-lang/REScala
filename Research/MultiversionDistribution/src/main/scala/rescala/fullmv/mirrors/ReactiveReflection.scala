package rescala.fullmv.mirrors

import java.util.concurrent.ConcurrentHashMap

import rescala.core.Reactive
import rescala.core._
import rescala.fullmv.tasks._
import rescala.fullmv.{FullMVEngine, FullMVState, FullMVStruct, FullMVTurn, TurnPhase}

trait ReactiveReflection[-P] extends Reactive[FullMVStruct] with ReactiveReflectionProxy[P] {
  self: RENamed =>
  val host: FullMVEngine
  def buffer(turn: FullMVTurn, value: P): Unit
  def submit(action: FullMVAction): Unit

  override def asyncIncrementFrame(turn: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Framing)
    turn.ensurePredecessorReplication()
    submit(Framing(turn, this))
  }
  override def asyncDecrementFrame(turn: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Framing)
    turn.ensurePredecessorReplication()
    submit(Deframing(turn, this))
  }
  override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Framing)
    turn.ensurePredecessorReplication()
    supersede.ensurePredecessorReplication()
    submit(SupersedeFraming(turn, this, supersede))
  }

  override def asyncDeframeReframe(turn: FullMVTurn, reframe: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Framing)
    turn.ensurePredecessorReplication()
    reframe.ensurePredecessorReplication()
    submit(DeframeReframing(turn, this, reframe))
  }

  override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    turn.ensurePredecessorReplication()
    submit(Notification(turn, this, changed = false))
  }
  override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    turn.ensurePredecessorReplication()
    followFrame.ensurePredecessorReplication()
    submit(NotificationWithFollowFrame(turn, this, changed = false, followFrame))
  }
  override def asyncNewValue(turn: FullMVTurn, value: P): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    turn.ensurePredecessorReplication()
    buffer(turn, value)
    submit(Notification(turn, this, changed = true))
  }
  override def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    turn.ensurePredecessorReplication()
    followFrame.ensurePredecessorReplication()
    buffer(turn, value)
    submit(NotificationWithFollowFrame(turn, this, changed = true, followFrame))
  }
}

class ReactiveReflectionImpl[P](override val host: FullMVEngine, var ignoreTurn: Option[FullMVTurn], initialState: FullMVState[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], rename: REName) extends Base[P, FullMVStruct](initialState, rename) with ReactiveReflection[P] {
  val _buffer = new ConcurrentHashMap[FullMVTurn, P]()
  override def buffer(turn: FullMVTurn, value: P): Unit = _buffer.put(turn, value)
  override def submit(action: FullMVAction): Unit = host.threadPool.submit(action)

  override protected[rescala] def reevaluate(input: ReIn) = {
    val turn = input.creation
    val value = _buffer.remove(turn)
    if(value == null) {
      if(ignoreTurn.contains(turn)){
        ignoreTurn = None
      } else {
        throw new AssertionError(s"$this was reevaluated for $turn but no value was buffered.")
      }
      input
    } else {
      if(ignoreTurn.contains(turn)) ignoreTurn = None
      input.withValue(value)
    }
  }
}
