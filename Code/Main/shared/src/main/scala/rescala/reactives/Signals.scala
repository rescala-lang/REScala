package rescala.reactives

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import rescala.reactives.Signals.Sstate

import scala.concurrent.{ExecutionContext, Future}

object Signals {
  type Sstate[T, S <: Struct] = S#State[Pulse[T], S]

  trait SignalResource[+T, S <: Struct] extends ReSource[S] with Interp[T, S] with Disconnectable[S] {
    override type Value <: Pulse[T]
    override def interpret(v: Value): T = v.get
  }

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

  import rescalaAPI.{Signal => Sig, Var}
  import rescalaAPI.Impls.{DerivedImpl, SignalImpl}


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  @cutOutOfUserComputation
  def static[T](dependencies: ReSource[S]*)
               (expr: StaticTicket[S] => T)
               (implicit ct: CreationTicket[S])
  : Sig[T] = {
    val derived = ct.create[Pulse[T], SignalImpl[T]](dependencies.toSet, Initializer.DerivedSignal, inite = true) {
      state => new SignalImpl[T](state, ignore2(expr), ct.rename, None)
    }
    signalAPI(derived)
  }

  def signalAPI[T](derived: rescalaAPI.Impls.SignalImpl[T]): Signal[T, S] = {
    new Signal[T, S] {
      override                  val rescalaAPI  : RescalaInterface[S]          = Signals.this.rescalaAPI
      override val innerDerived: Signals.SignalResource[T, S] = derived
    }
  }
  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  @cutOutOfUserComputation
  def dynamic[T](dependencies: ReSource[S]*)
                (expr: DynamicTicket[S] => T)
                (implicit ct: CreationTicket[S])
  : Sig[T] = {
    val staticDeps = dependencies.toSet
    val derived = ct.create[Pulse[T], SignalImpl[T]](staticDeps, Initializer.DerivedSignal, inite = true) {
      state => new SignalImpl[T](state, ignore2(expr), ct.rename, Some(staticDeps))
    }
    signalAPI(derived)
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
    static(los.map(_.innerDerived): _*) { t => fun(los.map(s => t.dependStatic(s.innerDerived))) }
  }

  @cutOutOfUserComputation
  def lift[A1, B](n1: Signal[A1, S])(fun: A1 => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1.innerDerived)(t => fun(t.dependStatic(n1.innerDerived)))
  }

  @cutOutOfUserComputation
  def lift[A1, A2, B](n1: Signal[A1, S], n2: Signal[A2, S])(fun: (A1, A2) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1.innerDerived, n2.innerDerived)(t => fun(t.dependStatic(n1.innerDerived), t.dependStatic(n2.innerDerived)))
  }
}

