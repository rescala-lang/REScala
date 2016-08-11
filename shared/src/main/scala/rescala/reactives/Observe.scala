package rescala.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph._
import rescala.propagation.{Committable, Turn}

import scala.util.Try

/**
  * Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
  * Currently this interface is only used to allow a removal of registered observer functions.
  *
  * @tparam S Struct type used for the propagation of the signal
  */
trait Observe[S <: Struct] {
  def remove()(implicit fac: Engine[S, Turn[S]]): Unit
}

object Observe {

  private class Obs[T, S <: Struct](bud: S#SporeP[T, Reactive[S]], dependency: Pulsing[T, S], fun: Try[T] => Unit) extends Base[T, S](bud) with Reactive[S] with Observe[S] {
    override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
      dependency.pulse(turn).toOptionTry(takeInitialValue = false).foreach(t => turn.schedule(once(this, t, fun)))
      if (turn.incoming(bud).isEmpty) ReevaluationResult.Dynamic(changed = false, DepDiff(Set.empty, Set(dependency)))
      else ReevaluationResult.Static(changed = false)
    }
    override def remove()(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { turn =>
      turn.updateIncoming(this.bud, Set.empty)
    }
  }

  def apply[T, S <: Struct](dependency: Pulsing[T, S])(fun: Try[T] => Unit)(implicit maybe: Ticket[S]): Observe[S] = {
    val incoming = Set[Reactive[S]](dependency)
    maybe(initTurn => initTurn.create(incoming) {
      val obs = new Obs(initTurn.bud[T, Reactive[S]](initialIncoming = incoming, transient = false), dependency, fun)
      dependency.pulse(initTurn).toOptionTry(takeInitialValue = true).foreach(t => initTurn.schedule(once(this, t, fun)))
      obs
    })
  }



  def once[V](self: AnyRef, value: Try[V], f: Try[V] => Unit): Committable = new Committable {
    override def release(implicit turn: Turn[_]): Unit = ()
    override def commit(implicit turn: Turn[_]): Unit = turn.observe(f(value))
    override def equals(obj: scala.Any): Boolean = self.equals(obj)
    override def hashCode(): Int = self.hashCode()
  }
}
