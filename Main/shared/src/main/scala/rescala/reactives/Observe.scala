package rescala.reactives

import rescala.core.{Interp, _}
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}

import scala.util.control.NonFatal

/**
  * Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
  * Currently this interface is only used to allow a removal of registered observer functions.
  *
  * @tparam S Struct type used for the propagation of the signal
  */
trait Observe[S <: Struct] {
  def remove()(implicit fac: Scheduler[S]): Unit
}

object Observe {

  private abstract class Obs[T, S <: Struct](state: S#State[Pulse[Nothing], S],
                                             dependency: Interp[T, S],
                                             fun: T => Unit,
                                             fail: Throwable => Unit,
                                             name: REName,
                                             removeIf: T => Boolean)
    extends Base[Pulse[Nothing], S](state, name) with Derived[S] with Observe[S] {
    this: DisconnectableImpl[S] =>

    override protected[rescala] def reevaluate(dt: ReIn): Rout = {
      try {
        val v = dt.dependStatic(dependency)
        if (removeIf(v)) {
          dt.trackDependencies(Set.empty)
        }
        else dt.withEffect(() => fun(v))
      } catch {
        case EmptySignalControlThrowable => dt
        case NonFatal(t)                 =>
          if (fail eq null) {
            throw new UnhandledFailureException(this, t)
          }
          else dt.withEffect(() => fail(t))
      }
    }
    override def remove()(implicit fac: Scheduler[S]): Unit = {
      disconnect()
    }
  }

  def strong[T, S <: Struct](dependency: Interp[T, S],
                             fireImmediately: Boolean)
                            (fun: T => Unit,
                             fail: Throwable => Unit,
                             removeIf: T => Boolean)
                            (implicit ct: CreationTicket[S]): Observe[S] = {
    ct.create[Pulse[Nothing], Obs[T, S]](Set(dependency),
                                         Initializer.Observer, fireImmediately) { state =>
      new Obs[T, S](state, dependency, fun, fail, ct.rename, removeIf) with DisconnectableImpl[S]
    }
  }

}
