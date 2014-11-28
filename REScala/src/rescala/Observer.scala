package rescala

import rescala.propagation.turns.{Commitable, Turn, TurnState}
import rescala.propagation.{EvaluationResult, Pulsing, Reactive}

/** Wrapper for an anonymous function to run in the afterCommit phase */
final case class Observer[T](fun: T => Unit, dependency: Pulsing[T]) extends Reactive with Commitable {
  val cached = TurnState[Option[T]](None, (_, x) => x)

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    cached.set(dependency.pulse.toOption)
    turn.markForCommit(this)
    EvaluationResult.Static(changed = false)
  }
  override def release(implicit turn: Turn): Unit = ()
  override def commit(implicit turn: Turn): Unit = cached.get.foreach(v => turn.afterCommit(fun(v)))
}
