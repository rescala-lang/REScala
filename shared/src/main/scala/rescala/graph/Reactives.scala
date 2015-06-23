package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.turns.{Ticket, Turn}
import rescala.synchronization.TurnLock

/** A Reactive is something that can be reevaluated */
trait Reactive {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def lock: TurnLock

  protected[rescala] def syncFactory: SynchronizationFactory

  final private[rescala] val level: Buffer[Int] = syncFactory.buffer(0, math.max, lock)

  final private[rescala] val outgoing: Buffer[Set[Reactive]] = syncFactory.buffer(Set(), Buffer.commitAsIs, lock)

  protected[rescala] def incoming(implicit turn: Turn): Set[Reactive]

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Base(
  final override protected[rescala] val syncFactory: SynchronizationFactory,
  knownDependencies: Set[Reactive] = Set.empty) extends {
  final override val lock: TurnLock =
    if (knownDependencies.size == 1) knownDependencies.head.lock
    else syncFactory.lock()
} with Reactive {

  def staticIncoming: Set[Reactive] = knownDependencies
}

class Reader[+P](pulses: Buffer[Pulse[P]]) {
  def pulse(implicit turn: Turn): Pulse[P] = pulses.get

  final def get(implicit turn: Turn): P = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
  protected[this] def strategy: (Pulse[P], Pulse[P]) => Pulse[P] = Buffer.transactionLocal[Pulse[P]]
  final protected[this] val pulses: Buffer[Pulse[P]] = syncFactory.buffer(Pulse.none, strategy, lock)

  final def pulse(implicit turn: Turn): Pulse[P] = pulses.get

  final val reader: Reader[P] = new Reader[P](pulses)
}


/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  override protected[this] def strategy: (Pulse[A], Pulse[A]) => Pulse[A] = Buffer.keepPulse

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn): A = {
    turn.accessDynamic(this)
    turn.useDependency(this)
    get(turn)
  }

  final def now(implicit maybe: Ticket): A = maybe {get(_)}

  final def get(implicit turn: Turn): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

