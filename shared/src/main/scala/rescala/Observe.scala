package rescala

import rescala.graph._
import rescala.turns.{Ticket, Turn}


trait Observe[S <: Spores] {
  def remove()(implicit maybe: Ticket[S]): Unit
}

object Observe {

  def apply[T, S <: Spores](dependency: Pulsing[T, S])(fun: T => Unit)(implicit maybe: Ticket[S]): Observe[S] =
    maybe(initTurn => initTurn.create(Set(dependency)) {
      val obs = new Base[S](initTurn.bufferFactory.bud(), Set(dependency)) with Reactive[S] with Observe[S] {
        override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
          turn.schedule(once(this, dependency.pulse.toOption, fun))
          ReevaluationResult.Static(changed = false)
        }
        override def remove()(implicit maybe: Ticket[S]): Unit = maybe(_.unregister(this)(dependency))
        override protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = staticIncoming
      }
      initTurn.schedule(once(obs, dependency.pulse(initTurn).keep.current, fun))
      obs
    })


  def once[V](self: AnyRef, value: Option[V], f: V => Unit): Committable = new Committable {
    override def release(implicit turn: Turn[_]): Unit = ()
    override def commit(implicit turn: Turn[_]): Unit = value.foreach(v => turn.observe(f(v)))
    override def equals(obj: scala.Any): Boolean = self.equals(obj)
    override def hashCode(): Int = self.hashCode()
  }


}
