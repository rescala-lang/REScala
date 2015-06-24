package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.turns.{Ticket, Turn}

/** A Reactive is something that can be reevaluated */
trait Reactive[S <: State] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def state: S

  protected[rescala] def lock: S#TLock

  final private[rescala] val level: S#TBuffer[Int] = state.buffer(0, math.max, lock)

  final private[rescala] val outgoing: S#TBuffer[Set[Reactive[S]]] = state.buffer(Set(), Buffer.commitAsIs, lock)

  protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]]

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Base[S <: State](
  final override protected[rescala] val state: S,
  knownDependencies: Set[Reactive[S]] = Set.empty[Reactive[S]]) extends {
  final override val lock: S#TLock =
    if (knownDependencies.size == 1) knownDependencies.head.lock
    else state.lock()
} with Reactive[S] {

  def staticIncoming: Set[Reactive[S]] = knownDependencies
}

class Reader[+P, S <: State](pulses: Buffer[Pulse[P]]) {
  def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get

  final def get(implicit turn: Turn[S]): P = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

/** A node that has nodes that depend on it */
trait Pulsing[+P, S <: State] extends Reactive[S] {
  protected[this] def strategy: (Pulse[P], Pulse[P]) => Pulse[P] = Buffer.transactionLocal[Pulse[P]]
  final protected[this] val pulses: S#TBuffer[Pulse[P]] = state.buffer(Pulse.none, strategy, lock)

  final def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get

  final val reader: Reader[P, S] = new Reader[P, S](pulses)
}


/** a node that has a current state */
trait Stateful[+A, S <: State] extends Pulsing[A, S] {
  override protected[this] def strategy: (Pulse[A], Pulse[A]) => Pulse[A] = Buffer.keepPulse

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn[S]): A = {
    turn.accessDynamic(this)
    turn.useDependency(this)
    get(turn)
  }

  final def now(implicit maybe: Ticket[S]): A = maybe {get(_)}

  final def get(implicit turn: Turn[S]): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

