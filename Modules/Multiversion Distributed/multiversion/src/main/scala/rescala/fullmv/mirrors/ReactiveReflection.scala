package rescala.fullmv.mirrors

import java.util.concurrent.ConcurrentHashMap
import rescala.core._
import rescala.fullmv.sgt.synchronization.SubsumableLockBundle
import rescala.fullmv.{FullMVBundle, FullMvStateBundle, TurnImplBundle, TurnPhase}
import rescala.fullmv.tasks._

trait ReactiveReflectionBundle extends FullMVBundle {
  self: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle =>

  trait ReactiveReflection[-P] extends Derived with ReactiveReflectionProxy[P] {
    type State[V] = self.State[V]
    val host: FullMVEngine
    def buffer(turn: FullMVTurn, value: P): Unit
    def submit(action: FullMVAction): Unit

    override def asyncIncrementFrame(turn: FullMVTurn): Unit = {
      submit(new Framing(turn, this) {
        override def doCompute(): Unit = {
          this.turn.newBranchFromRemote(TurnPhase.Framing)
          super.doCompute()
        }
        override def toString: String = "Remote" + super.toString
      })
    }

    override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = {
      submit(new SupersedeFraming(turn, this, supersede) {
        override def doCompute(): Unit = {
          this.turn.newBranchFromRemote(TurnPhase.Framing)
          super.doCompute()
        }
        override def toString: String = "Remote" + super.toString
      })
    }

    override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = {
      submit(new Notification(turn, this, changed = false) {
        override def doCompute(): Unit = {
          this.turn.newBranchFromRemote(TurnPhase.Executing)
          super.doCompute()
        }
        override def toString: String = "Remote" + super.toString
      })
    }

    override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = {
      submit(new NotificationWithFollowFrame(turn, this, changed = false, followFrame) {
        override def doCompute(): Unit = {
          this.turn.newBranchFromRemote(TurnPhase.Executing)
          super.doCompute()
        }
        override def toString: String = "Remote" + super.toString
      })
    }

    override def asyncNewValue(turn: FullMVTurn, value: P): Unit = {
      submit(new Notification(turn, this, changed = true) {
        override def doCompute(): Unit = {
          this.turn.newBranchFromRemote(TurnPhase.Executing)
          buffer(this.turn, value)
          super.doCompute()
        }
        override def toString: String = "Remote" + super.toString
      })
    }

    override def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit = {
      submit(new NotificationWithFollowFrame(turn, this, changed = true, followFrame) {
        override def doCompute(): Unit = {
          this.turn.newBranchFromRemote(TurnPhase.Executing)
          buffer(this.turn, value)
          super.doCompute()
        }
        override def toString: String = "Remote" + super.toString
      })
    }
  }

  class ReactiveReflectionImpl[P](
      override val host: FullMVEngine,
      var ignoreTurn: Option[FullMVTurn],
      initialState: FullMVState[P, FullMVTurn],
      rename: ReInfo
  ) extends Base[State, P](initialState, rename)
      with ReactiveReflection[P] {
    val _buffer                                           = new ConcurrentHashMap[FullMVTurn, P]()
    override def buffer(turn: FullMVTurn, value: P): Unit = { _buffer.put(turn, value); () }
    override def submit(action: FullMVAction): Unit       = { host.threadPool.submit(action); () }

    override protected[rescala] def commit(base: Value): Value = throw new IllegalStateException(
      "TODO: this is not implemented, commit is a new method that enables reactives to change their value on commit (such as events dropping back to no value). Not sure how to map that to reactive reflections?"
    )

    override protected[rescala] def reevaluate(input: ReIn): ReevTicket[State, P] = {
      val turn  = input.tx
      val value = _buffer.remove(turn)
      if (value == null) {
        if (ignoreTurn.contains(turn.initializer)) {
          ignoreTurn = None
        } else {
          throw new AssertionError(s"$this was reevaluated for $turn but no value was buffered.")
        }
        input
      } else {
        if (ignoreTurn.contains(turn.initializer)) ignoreTurn = None
        input.withValue(value)
      }
    }
  }
}
