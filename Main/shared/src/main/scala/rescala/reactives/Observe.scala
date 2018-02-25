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
  def dereferenceAllStrongObserversWARNINGonlyUseThisIfYouKnowWhatYouAreDoing(): Unit = strongObserveReferences.clear()

  private val strongObserveReferences = scala.collection.mutable.HashSet[Observe[_]]()

  private abstract class Obs[T, S <: Struct]
  (bud: S#State[Pulse[Nothing], S], dependency: Interp[T, S], fun: T => Unit, fail: Throwable => Unit, name: REName)
    extends Base[Pulse[Nothing], S](bud, name) with Reactive[S] with Observe[S] {
    this: DisconnectableImpl[S] =>

    override protected[rescala] def reevaluate(dt: ReIn): Rout = {
      try {
        val v = dt.dependStatic(dependency)
        dt.withEffect(() => fun(v))
      } catch {
        case EmptySignalControlThrowable => dt
        case NonFatal(t) =>
          if (fail eq null) {
            throw new UnhandledFailureException(this, t)
          }
          else dt.withEffect(() => fail(t))
      }
    }
    override def remove()(implicit fac: Scheduler[S]): Unit = {
      disconnect()
      strongObserveReferences.synchronized(strongObserveReferences.remove(this))
    }
  }

  def weak[T, S <: Struct](dependency: Interp[T, S],
                           fireImmediately: Boolean)
                          (fun: T => Unit,
                           fail: Throwable => Unit)
                          (implicit ct: CreationTicket[S]): Observe[S] = {
    ct(initTurn => initTurn.create[Pulse[Nothing], Obs[T, S]](Set(dependency),
      Initializer.Observer, fireImmediately) { state =>
      new Obs[T, S](state, dependency, fun, fail, ct.rename) with DisconnectableImpl[S]
    })
  }

  def strong[T, S <: Struct](dependency: Interp[T, S], fireImmediately: Boolean)(fun: T => Unit, fail: Throwable => Unit)(implicit maybe: CreationTicket[S]): Observe[S] = {
    val obs = weak(dependency, fireImmediately)(fun, fail)
    strongObserveReferences.synchronized(strongObserveReferences.add(obs))
    obs
  }

}
