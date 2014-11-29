package rescala

import rescala.propagation.turns.{Commitable, Turn, Buffer}
import rescala.propagation.{Ticket, EvaluationResult, Pulsing, Reactive}


trait Observe {
  def remove()(implicit maybe: Ticket): Unit
}

object Observe {

  def apply[T](dependency: Pulsing[T])(fun: T => Unit)(implicit maybe: Ticket): Observe =
    maybe(_.create(Set(dependency))(new Reactive with Commitable with Observe {
      val cached = Buffer[Option[T]](None, (_, x) => x)

      override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
        cached.set(dependency.pulse.toOption)
        turn.plan(this)
        EvaluationResult.Static(changed = false)
      }

      override protected[rescala] def release(implicit turn: Turn): Unit = ()
      override protected[rescala] def commit(implicit turn: Turn): Unit = cached.get.foreach(v => turn.afterCommit(fun(v)))

      override def remove()(implicit maybe: Ticket): Unit = maybe(_.unregister(this)(dependency))
    }))

}
