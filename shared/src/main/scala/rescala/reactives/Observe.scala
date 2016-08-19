package rescala.reactives

import java.util.concurrent.ConcurrentHashMap

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

  val strongObserveReferences = new ConcurrentHashMap[Observe[_], Boolean]()

  private class Obs[T, S <: Struct](bud: S#SporeP[T, Reactive[S]], dependency: Pulsing[T, S], fun: Try[T] => Unit) extends Base[T, S](bud) with Reactive[S] with Observe[S] with Disconnectable[S] {
    override protected[rescala] def computeReevaluationResult()(implicit turn: Turn[S]): ReevaluationResult[S] = {
      dependency.pulse(turn).toOptionTry(takeInitialValue = false).foreach(t => turn.schedule(once(this, t, fun)))
      ReevaluationResult.Static(changed = false)
    }
    override def remove()(implicit fac: Engine[S, Turn[S]]): Unit = {
      disconnect()
      strongObserveReferences.remove(this: Observe[_])
    }
  }

  def apply[T, S <: Struct](dependency: Pulsing[T, S])(fun: Try[T] => Unit)(implicit maybe: Ticket[S]): Observe[S] = {
    val incoming = Set[Reactive[S]](dependency)
    val obs = maybe(initTurn => initTurn.create(incoming) {
      val obs = new Obs(initTurn.bud[T, Reactive[S]](initialIncoming = incoming, transient = false), dependency, fun)
      dependency.pulse(initTurn).toOptionTry(takeInitialValue = true).foreach(t => initTurn.schedule(once(this, t, fun)))
      obs
    })
    strongObserveReferences.put(obs, true)
    obs
  }


  def once[V](self: AnyRef, value: Try[V], f: Try[V] => Unit): Committable = new Committable {
    override def release(implicit turn: Turn[_]): Unit = ()
    override def commit(implicit turn: Turn[_]): Unit = turn.observe(f(value))
    override def equals(obj: scala.Any): Boolean = self.equals(obj)
    override def hashCode(): Int = self.hashCode()
  }
}
