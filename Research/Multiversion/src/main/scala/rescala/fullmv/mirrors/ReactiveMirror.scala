package rescala.fullmv.mirrors


import rescala.core.Node.InDep
import rescala.core._
import rescala.fullmv._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object ReactiveMirror {
  def apply[A](reactive: ReadableReactive[A, FullMVStruct], turn: FullMVTurn, reflectionProxy: ReactiveReflectionProxy[A], reflectionIsTransient: Boolean, rename: REName): (Array[(FullMVTurn, A)], Option[FullMVTurn]) = {
    assert(turn.host == reactive.state.host, s"mirror installation for $reactive on ${reactive.state.host} with $turn from different ${turn.host}")
    def getValue(turn: FullMVTurn): A = turn.staticAfter(reactive)
    val mirror = new ReactiveMirror(getValue, reflectionProxy, turn.timeout, rename)

    val (successorWrittenVersions, maybeFirstFrame) = reactive.state.discover(turn, mirror)
    val mustAddBaseValue = !reflectionIsTransient && (successorWrittenVersions.isEmpty || successorWrittenVersions.head != turn)
    var idx = if(mustAddBaseValue) 1 else 0
    val initValues = new Array[(FullMVTurn, A)](successorWrittenVersions.size + idx)
    if(mustAddBaseValue) initValues(0) = turn -> getValue(turn)
    for(succ <- successorWrittenVersions) {
      initValues(idx) = succ -> getValue(succ)
      idx += 1
    }
    (initValues, maybeFirstFrame)
  }
}

class ReactiveMirror[A](val getValue: FullMVTurn => A, val reflectionProxy: ReactiveReflectionProxy[A], val timeout: Duration, rename: REName) extends RENamed(rename) with Reactive[FullMVStruct] with FullMVState[Nothing, FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]] {
  override type Value = Nothing
  override protected[rescala] val state = this
  override val host: FullMVEngine = null
  override def incrementFrame(txn: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    val add = txn.addRemoteBranch(TurnPhase.Framing)
    reflectionProxy.asyncIncrementFrame(txn)
    Await.result(add, timeout)
    FramingBranchResult.FramingBranchEnd
  }
  override def incrementSupersedeFrame(txn: FullMVTurn, supersede: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    val add = txn.addRemoteBranch(TurnPhase.Framing)
    reflectionProxy.asyncIncrementSupersedeFrame(txn, supersede)
    Await.result(add, timeout)
    FramingBranchResult.FramingBranchEnd
  }
  override def notify(txn: FullMVTurn, changed: Boolean): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
    val add = txn.addRemoteBranch(TurnPhase.Executing)
    if(changed) {
      reflectionProxy.asyncNewValue(txn, getValue(txn))
    } else {
      reflectionProxy.asyncResolvedUnchanged(txn)
    }
    Await.result(add, timeout)
    NotificationResultAction.GlitchFreeReadyButQueued
  }
  override def notifyFollowFrame(txn: FullMVTurn, changed: Boolean, followFrame: FullMVTurn): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
    val add = txn.addRemoteBranch(TurnPhase.Executing)
    if(changed) {
      reflectionProxy.asyncNewValueFollowFrame(txn, getValue(txn), followFrame)
    } else {
      reflectionProxy.asyncResolvedUnchangedFollowFrame(txn, followFrame)
    }
    Await.result(add, timeout)
    NotificationResultAction.GlitchFreeReadyButQueued
  }
  override def latestValue: Value = ???
  override def reevIn(turn: FullMVTurn): Nothing = ???
  override def reevOut(turn: FullMVTurn, maybeValue: Option[Value]): NotificationResultAction.NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] = ???
  override def dynamicBefore(txn: FullMVTurn): Nothing = ???
  override def staticBefore(txn: FullMVTurn): Nothing = ???
  override def dynamicAfter(txn: FullMVTurn): Nothing = ???
  override def staticAfter(txn: FullMVTurn): Nothing = ???
  override def discover(txn: FullMVTurn, add: Reactive[FullMVStruct]): (Seq[FullMVTurn], Option[FullMVTurn]) = ???
  override def drop(txn: FullMVTurn, remove: Reactive[FullMVStruct]): (Seq[FullMVTurn], Option[FullMVTurn]) = ???
  override def retrofitSinkFrames(successorWrittenVersions: Seq[FullMVTurn], maybeSuccessorFrame: Option[FullMVTurn], arity: Int): Unit = ???

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = ???
}
