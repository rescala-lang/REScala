package rescala

import rescala.graph.{Enlock, Committable, ReevaluationResult, Pulsing, Reactive}
import rescala.turns.{Ticket, Turn}
import rescala.graph.ReactiveImpl
import rescala.graph.StaticBuffer


trait Observe {
  def remove()(implicit maybe: Ticket): Unit
}

object Observe {

  def apply[T](dependency: Pulsing[T])(fun: T => Unit)(implicit maybe: Ticket): Observe =
    maybe(initTurn => initTurn.create(Set(dependency)) {
      val obs = new ReactiveImpl(initTurn.engine, Set(dependency)) with Reactive with Observe {
        override protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult = {
          turn.schedule(once(this, dependency.pulse.toOption, fun))
          ReevaluationResult.Static(changed = false)
        }
        override def remove()(implicit maybe: Ticket): Unit = maybe(_.unregister(this)(dependency))
        override protected[rescala] val incoming = new StaticBuffer(staticIncoming)
      }
      initTurn.schedule(once(obs, dependency.pulse(initTurn).keep.current, fun))
      obs
    })


  def once[V](self: AnyRef, value: Option[V], f: V => Unit): Committable = new Committable {
    override def release(implicit turn: Turn): Unit = ()
    override def commit(implicit turn: Turn): Unit = value.foreach(v => turn.observe(f(v)))
    override def equals(obj: scala.Any): Boolean = self.equals(obj)
    override def hashCode(): Int = self.hashCode()
  }


}
