package reactives.fullmv.mirrors

import reactives.core._
import reactives.fullmv._
import reactives.fullmv.sgt.synchronization.SubsumableLockBundle
import reactives.fullmv.tasks.TaskBundle

import scala.annotation.nowarn
import scala.concurrent.duration.Duration

trait ReactiveMirrorBundle extends FullMVTurnReflectionBundle {
  self: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle =>

  object ReactiveMirror {
    def apply[A](reactive: ReSource.of[State], turn: FullMVTurn, reflectionIsTransient: Boolean, rename: ReInfo)(
        toPulse: reactive.Value => A,
        reflectionProxy: ReactiveReflectionProxy[A]
    ): (List[(FullMVTurn, A)], Option[FullMVTurn]) = {
      assert(
        turn.host == reactive.state.host,
        s"mirror installation for $reactive on ${reactive.state.host} with $turn from different ${turn.host}"
      )

      def getValue(turn: FullMVTurn): A = toPulse(reactive.state.staticAfter(turn))

      val mirror = new ReactiveMirror(getValue, reflectionProxy, turn.host.timeout, rename)

      val ownValue                                    = toPulse(reactive.state.dynamicAfter(turn))
      val (successorWrittenVersions, maybeFirstFrame) = reactive.state.discover(turn, mirror)
      val mustAddBaseValue =
        !reflectionIsTransient && (successorWrittenVersions.isEmpty || successorWrittenVersions.head != turn)
      val initValues = successorWrittenVersions.map { succ =>
        succ -> getValue(succ)
      }
      (if (mustAddBaseValue) ((turn, ownValue) :: initValues) else initValues, maybeFirstFrame)
    }
  }

  @nowarn
  class ReactiveMirror[A](
      val getValue: FullMVTurn => A,
      val reflectionProxy: ReactiveReflectionProxy[A],
      val timeout: Duration,
      override val info: ReInfo
  ) extends Derived
      with FullMVState[Nothing, FullMVTurn] {
    type State[V]       = self.State[V]
    override type Value = Nothing
    override protected[reactives] val state = this
    override def toString: String           = s"Mirror${info.description}"
    override val host: FullMVEngine         = null
    override def incrementFrame(txn: FullMVTurn): FramingBranchResult[FullMVTurn, Derived.of[State]] = {
      FullMVUtil.myAwait(txn.addRemoteBranch(TurnPhase.Framing), timeout)
      reflectionProxy.asyncIncrementFrame(txn)
      FramingBranchResult.FramingBranchEnd
    }
    override def incrementSupersedeFrame(
        txn: FullMVTurn,
        supersede: FullMVTurn
    ): FramingBranchResult[FullMVTurn, Derived.of[State]] = {
      FullMVUtil.myAwait(txn.addRemoteBranch(TurnPhase.Framing), timeout)
      reflectionProxy.asyncIncrementSupersedeFrame(txn, supersede)
      FramingBranchResult.FramingBranchEnd
    }
    override def notify(
        txn: FullMVTurn,
        changed: Boolean
    ): (Boolean, NotificationBranchResult[FullMVTurn, Derived.of[State]]) = {
      FullMVUtil.myAwait(txn.addRemoteBranch(TurnPhase.Executing), timeout)
      if (changed) {
        reflectionProxy.asyncNewValue(txn, getValue(txn))
      } else {
        reflectionProxy.asyncResolvedUnchanged(txn)
      }
      false -> NotificationBranchResult.DoNothing
    }
    override def notifyFollowFrame(
        txn: FullMVTurn,
        changed: Boolean,
        followFrame: FullMVTurn
    ): (Boolean, NotificationBranchResult[FullMVTurn, Derived.of[State]]) = {
      FullMVUtil.myAwait(txn.addRemoteBranch(TurnPhase.Executing), timeout)
      if (changed) {
        reflectionProxy.asyncNewValueFollowFrame(txn, getValue(txn), followFrame)
      } else {
        reflectionProxy.asyncResolvedUnchangedFollowFrame(txn, followFrame)
      }
      false -> NotificationBranchResult.DoNothing
    }
    override def latestValue: Value                = ???
    override def reevIn(turn: FullMVTurn): Nothing = ???
    override def reevOut(
        turn: FullMVTurn,
        maybeValue: Option[Value],
        unchange: Value => Value
    ): NotificationBranchResult.ReevOutBranchResult[FullMVTurn, Derived.of[State]] = ???

    override def dynamicBefore(txn: FullMVTurn): Nothing                                                   = ???
    override def staticBefore(txn: FullMVTurn): Nothing                                                    = ???
    override def dynamicAfter(txn: FullMVTurn): Nothing                                                    = ???
    override def staticAfter(txn: FullMVTurn): Nothing                                                     = ???
    override def discover(txn: FullMVTurn, add: Derived.of[State]): (List[FullMVTurn], Option[FullMVTurn]) = ???
    override def drop(txn: FullMVTurn, remove: Derived.of[State]): (List[FullMVTurn], Option[FullMVTurn])  = ???
    override def retrofitSinkFrames(
        successorWrittenVersions: Seq[FullMVTurn],
        maybeSuccessorFrame: Option[FullMVTurn],
        arity: Int
    ): Seq[FullMVTurn] = ???

    override def commit(base: Value): Value = ???

    override protected[reactives] def reevaluate(input: ReIn) = ???
  }
}
