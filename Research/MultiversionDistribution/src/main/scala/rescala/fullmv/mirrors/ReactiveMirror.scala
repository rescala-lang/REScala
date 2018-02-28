package rescala.fullmv.mirrors

import rescala.core.Reactive
import rescala.core._
import rescala.fullmv._

import scala.concurrent.duration.Duration

object ReactiveMirror {
  def apply[A](reactive: ReSource[FullMVStruct], turn: FullMVTurn, reflectionIsTransient: Boolean, rename: REName)(toPulse: reactive.Value => A, reflectionProxy: ReactiveReflectionProxy[A]): (Array[(FullMVTurn, A)], Option[FullMVTurn]) = {
    assert(turn.host == reactive.state.host, s"mirror installation for $reactive on ${reactive.state.host} with $turn from different ${turn.host}")
    def getValue(turn: FullMVTurn): A = toPulse(reactive.state.staticAfter(turn))
    val mirror = new ReactiveMirror(getValue, reflectionProxy, turn.host.timeout, rename)

    val ownValue = toPulse(reactive.state.dynamicAfter(turn))
    val (successorWrittenVersions, maybeFirstFrame) = reactive.state.discover(turn, mirror)
    val mustAddBaseValue = !reflectionIsTransient && (successorWrittenVersions.isEmpty || successorWrittenVersions.head != turn)
    var idx = if(mustAddBaseValue) 1 else 0
    val initValues = new Array[(FullMVTurn, A)](successorWrittenVersions.size + idx)
    if(mustAddBaseValue) initValues(0) = turn -> ownValue
    for(succ <- successorWrittenVersions) {
      initValues(idx) = succ -> getValue(succ)
      idx += 1
    }
    (initValues, maybeFirstFrame)
  }
}

class ReactiveMirror[A](val getValue: FullMVTurn => A, val reflectionProxy: ReactiveReflectionProxy[A], val timeout: Duration, rename: REName) extends RENamed(rename) with Reactive[FullMVStruct] with FullMVState[Nothing, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]] {
  override type Value = Nothing
  override protected[rescala] val state = this
  override val host: FullMVEngine = null
  override def incrementFrame(txn: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    FullMVEngine.myAwait(txn.addRemoteBranch(TurnPhase.Framing), timeout)
    reflectionProxy.asyncIncrementFrame(txn)
    FramingBranchResult.FramingBranchEnd
  }
  override def decrementFrame(txn: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    FullMVEngine.myAwait(txn.addRemoteBranch(TurnPhase.Framing), timeout)
    reflectionProxy.asyncDecrementFrame(txn)
    FramingBranchResult.FramingBranchEnd
  }
  override def incrementSupersedeFrame(txn: FullMVTurn, supersede: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    FullMVEngine.myAwait(txn.addRemoteBranch(TurnPhase.Framing), timeout)
    reflectionProxy.asyncIncrementSupersedeFrame(txn, supersede)
    FramingBranchResult.FramingBranchEnd
  }
  override def decrementReframe(txn: FullMVTurn, reframe: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    FullMVEngine.myAwait(txn.addRemoteBranch(TurnPhase.Framing), timeout)
    reflectionProxy.asyncDeframeReframe(txn, reframe)
    FramingBranchResult.FramingBranchEnd
  }
  override def notify(txn: FullMVTurn, changed: Boolean): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
    FullMVEngine.myAwait(txn.addRemoteBranch(TurnPhase.Executing), timeout)
    if(changed) {
      reflectionProxy.asyncNewValue(txn, getValue(txn))
    } else {
      reflectionProxy.asyncResolvedUnchanged(txn)
    }
    NotificationResultAction.GlitchFreeReadyButQueued
  }
  override def notifyFollowFrame(txn: FullMVTurn, changed: Boolean, followFrame: FullMVTurn): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
    FullMVEngine.myAwait(txn.addRemoteBranch(TurnPhase.Executing), timeout)
    if(changed) {
      reflectionProxy.asyncNewValueFollowFrame(txn, getValue(txn), followFrame)
    } else {
      reflectionProxy.asyncResolvedUnchangedFollowFrame(txn, followFrame)
    }
    NotificationResultAction.GlitchFreeReadyButQueued
  }
  override def latestValue: Value = ???
  override def reevIn(turn: FullMVTurn): Nothing = ???
  override def reevOut(turn: FullMVTurn, maybeValue: Option[Value]): NotificationResultAction.ReevOutResult[FullMVTurn, Reactive[FullMVStruct]] = ???
  override def dynamicBefore(txn: FullMVTurn): Nothing = ???
  override def staticBefore(txn: FullMVTurn): Nothing = ???
  override def dynamicAfter(txn: FullMVTurn): Nothing = ???
  override def staticAfter(txn: FullMVTurn): Nothing = ???
  override def discover(txn: FullMVTurn, add: Reactive[FullMVStruct]): (Seq[FullMVTurn], Option[FullMVTurn]) = ???
  override def drop(txn: FullMVTurn, remove: Reactive[FullMVStruct]): (Seq[FullMVTurn], Option[FullMVTurn]) = ???
  override def retrofitSinkFrames(successorWrittenVersions: Seq[FullMVTurn], maybeSuccessorFrame: Option[FullMVTurn], arity: Int): Unit = ???


  override protected[rescala] def reevaluate(input: ReIn) = ???
}
