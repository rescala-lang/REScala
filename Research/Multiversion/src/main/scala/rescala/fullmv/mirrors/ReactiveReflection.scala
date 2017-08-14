package rescala.fullmv.mirrors

import java.util.concurrent.ConcurrentHashMap

import rescala.core.Node.InDep
import rescala.core._
import rescala.fullmv.tasks._
import rescala.fullmv.{FullMVEngine, FullMVState, FullMVStruct, FullMVTurn, TurnPhase}



trait ReactiveReflection[-P] extends WriteableReactive[P, FullMVStruct] with Reactive[FullMVStruct] with ReactiveReflectionProxy[P] {
  self: RENamed =>
  def submit(action: FullMVAction): Unit = FullMVEngine.threadPool.submit(action)

  override def asyncIncrementFrame(turn: FullMVTurn): Unit = {
    turn.activeBranchDifferential(TurnPhase.Framing, 1)
    submit(Framing(turn, this))
  }
  override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = {
    turn.activeBranchDifferential(TurnPhase.Framing, 1)
    submit(SupersedeFraming(turn, this, supersede))
  }
  override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = {
    turn.activeBranchDifferential(TurnPhase.Executing, 1)
    submit(Notification(turn, this, changed = false))
  }
  override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = {
    turn.activeBranchDifferential(TurnPhase.Executing, 1)
    submit(NotificationWithFollowFrame(turn, this, changed = false, followFrame))
  }
  override def asyncNewValue(turn: FullMVTurn, value: P): Unit = {
    turn.activeBranchDifferential(TurnPhase.Executing, 1)
    buffer(turn, value)
    submit(Notification(turn, this, changed = true))
  }
  override def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit = {
    turn.activeBranchDifferential(TurnPhase.Executing, 1)
    buffer(turn, value)
    submit(NotificationWithFollowFrame(turn, this, changed = true, followFrame))
  }

  def buffer(turn: FullMVTurn, value: P): Unit
}

class ReactiveReflectionImpl[P](var ignoreTurn: Option[FullMVTurn], initialState: FullMVState[Pulse[P], FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]], rename: REName) extends Base[P, FullMVStruct](initialState, rename) with ReactiveReflection[Pulse[P]] {
  val _buffer = new ConcurrentHashMap[FullMVTurn, Pulse[P]]()
  override def buffer(turn: FullMVTurn, value: Pulse[P]): Unit = _buffer.put(turn, value)

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = {
    val value = _buffer.remove(turn)
    if(value == null) {
      if(ignoreTurn.contains(turn)){
        ignoreTurn = None
      } else {
        throw new AssertionError(s"$this was reevaluated for $turn but no value was buffered.")
      }
      ReevaluationResult.Static(turn, this, Pulse.NoChange: Pulse[P], indeps)
    } else {
      if(ignoreTurn.contains(turn)) ignoreTurn = None
      ReevaluationResult.Static(turn, this, value, indeps)
    }
  }
}
