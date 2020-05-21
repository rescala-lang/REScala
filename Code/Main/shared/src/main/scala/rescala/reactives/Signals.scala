package rescala.reactives

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import rescala.reactives.Signals.Sstate

import scala.concurrent.{ExecutionContext, Future}

object Signals {
  type Sstate[T, S <: Struct] = S#State[Pulse[T], S]

  object MapFuncImpl {def apply[T1, A](value: T1, mapper: T1 => A): A = mapper(value)}

  class Diff[+A](val from: Pulse[A], val to: Pulse[A]) {

    def _1: A = from.get
    def _2: A = to.get
    def pair: (A, A) = {
      try {
        val right = to.get
        val left  = from.get
        left -> right
      } catch {
        case EmptySignalControlThrowable => throw new NoSuchElementException(s"Can not convert $this to pair")
      }
    }

    override def toString: String = s"Diff($from, $to)"
  }

  object Diff {
    def apply[A](from: Pulse[A], to: Pulse[A]): Diff[A] = new Diff(from, to)
    def unapply[A](arg: Diff[A]): Option[(A, A)] = arg.from match {
      case Pulse.Value(v1) => arg.to match {
        case Pulse.Value(v2) => Some((v1, v2))
        case _               => None
      }
      case _               => None
    }
  }

}

/** Functions to construct signals, you probably want to use signal expressions in [[rescala.interface.RescalaInterface.Signal]] for a nicer API. */
trait Signals[S <: Struct] {

  val rescalaAPI: RescalaInterface[S]
  private def ignore2[Tick, Current, Res](f: Tick => Res): (Tick, Current) => Res = (ticket, _) => f(ticket)

  import rescalaAPI.{Signal => Sig, Var, SignalImpl}


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  @cutOutOfUserComputation
  def static[T](dependencies: ReSource[S]*)
               (expr: StaticTicket[S] => T)
               (implicit ct: CreationTicket[S])
  : Sig[T] = {
    ct.create[Pulse[T], SignalImpl[T]](dependencies.toSet, Initializer.DerivedSignal, inite = true) {
      state => new SignalImpl[T](state, ignore2(expr), ct.rename, None, rescalaAPI)
    }
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  @cutOutOfUserComputation
  def dynamic[T](dependencies: ReSource[S]*)
                (expr: DynamicTicket[S] => T)
                (implicit ct: CreationTicket[S])
  : Sig[T] = {
    val staticDeps = dependencies.toSet
    ct.create[Pulse[T], SignalImpl[T]](staticDeps, Initializer.DerivedSignal, inite = true) {
      state => new SignalImpl[T](state, ignore2(expr), ct.rename, Some(staticDeps), rescalaAPI)
    }
  }

  /** converts a future to a signal */
  @cutOutOfUserComputation
  def fromFuture[A](fut: Future[A])(implicit fac: Scheduler[S], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A] = rescalaAPI.Var.empty[A]
    fut.onComplete { res => fac.forceNewTransaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(t)) }
    v
  }

  @cutOutOfUserComputation
  def lift[A, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit maybe: CreationTicket[S]): Signal[R, S] = {
    static(los: _*) { t => fun(los.map(s => t.dependStatic(s))) }
  }

  @cutOutOfUserComputation
  def lift[A1, B](n1: Signal[A1, S])(fun: A1 => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1)(t => fun(t.dependStatic(n1)))
  }

  @cutOutOfUserComputation
  def lift[A1, A2, B](n1: Signal[A1, S], n2: Signal[A2, S])(fun: (A1, A2) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1, n2)(t => fun(t.dependStatic(n1), t.dependStatic(n2)))
  }
}

trait SignalDefaultImplementations[S <: Struct] {
  self: RescalaInterface[S] =>

  class SignalImpl[T](initial: Sstate[T, S],
                      expr: (DynamicTicket, () => T) => T,
                      name: REName,
                      staticDeps: Option[Set[ReSource]],
                      override val rescalaAPI: RescalaInterface[S])
    extends Base[Pulse[T], S](initial, name) with Derived[S] with Signal[T] with DisconnectableImpl[S] {

    private def computeNewValue(rein: ReevTicket[Pulse[T], S], newValue: () => T): ReevTicket[Pulse[T], S] = {
      val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue(), rein.before))
      if (newPulse.isChange) rein.withValue(newPulse) else rein
    }

    override protected[rescala] def reevaluate(rein: ReIn): Rout = guardReevaluate(rein) {
      val rein2 = staticDeps.fold(rein.trackStatic())(rein.trackDependencies)
      computeNewValue(rein2, () => expr(rein2, () => rein2.before.get))
    }
  }
}
