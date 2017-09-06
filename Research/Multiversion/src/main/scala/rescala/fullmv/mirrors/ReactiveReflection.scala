package rescala.fullmv.mirrors

import java.util.concurrent.ConcurrentHashMap

import rescala.core.Reactive
import rescala.core._
import rescala.fullmv.tasks._
import rescala.fullmv.{FullMVEngine, FullMVState, FullMVStruct, FullMVTurn, TurnPhase}



trait ReactiveReflection[-P] extends WriteableReactive[P, FullMVStruct] with Reactive[FullMVStruct] with ReactiveReflectionProxy[P] {
  self: RENamed =>
  def buffer(turn: FullMVTurn, value: P): Unit
  def submit(action: FullMVAction): Unit

  override def asyncIncrementFrame(turn: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Framing)
    submit(Framing(turn, this))
  }
  override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Framing)
    submit(SupersedeFraming(turn, this, supersede))
  }
  override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    submit(Notification(turn, this, changed = false))
  }
  override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    submit(NotificationWithFollowFrame(turn, this, changed = false, followFrame))
  }
  override def asyncNewValue(turn: FullMVTurn, value: P): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    buffer(turn, value)
    submit(Notification(turn, this, changed = true))
  }
  override def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit = {
    turn.newBranchFromRemote(TurnPhase.Executing)
    buffer(turn, value)
    submit(NotificationWithFollowFrame(turn, this, changed = true, followFrame))
  }
}

class ReactiveReflectionImpl[P](val host: FullMVEngine, var ignoreTurn: Option[FullMVTurn], initialState: FullMVState[Pulse[P], FullMVTurn, Reactive[FullMVStruct], Reactive[FullMVStruct]], rename: REName) extends Base[P, FullMVStruct](initialState, rename) with ReactiveReflection[Pulse[P]] {
  val _buffer = new ConcurrentHashMap[FullMVTurn, Pulse[P]]()
  override def buffer(turn: FullMVTurn, value: Pulse[P]): Unit = _buffer.put(turn, value)
  override def submit(action: FullMVAction): Unit = host.threadPool.submit(action)

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[Reactive[FullMVStruct]]): ReevaluationResult[FullMVStruct] = {
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
