package rescala

import rescala.propagation.turns.creation.MaybeTurn
import rescala.propagation.turns.{Commitable, Turn, TurnState}
import rescala.propagation.{EvaluationResult, Pulsing, Reactive}


trait Observe {
  def remove()(implicit maybe: MaybeTurn): Unit
}

object Observe {

  def apply[T](dependency: Pulsing[T])(fun: T => Unit)(implicit maybe: MaybeTurn): Observe =
    maybe(_.create(Set(dependency))(new Reactive with Commitable with Observe {
      val cached = TurnState[Option[T]](None, (_, x) => x)

      override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
        cached.set(dependency.pulse.toOption)
        turn.markForCommit(this)
        EvaluationResult.Static(changed = false)
      }

      override protected[rescala] def release(implicit turn: Turn): Unit = ()
      override protected[rescala] def commit(implicit turn: Turn): Unit = cached.get.foreach(v => turn.afterCommit(fun(v)))

      override def remove()(implicit maybe: MaybeTurn): Unit = maybe(_.unregister(this)(dependency))
    }))

}
