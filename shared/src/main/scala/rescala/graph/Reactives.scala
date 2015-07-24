package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.turns.{Ticket, Turn}

/** A Reactive is something that can be reevaluated */
trait Reactive[S <: Spores] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def bud: S#Bud

  protected[rescala] def lock: S#TLock = bud.lock()

  final private[rescala] val level: S#TBuffer[Int] = bud.buffer(0, math.max)

  final private[rescala] val outgoing: S#TBuffer[Set[Reactive[S]]] = bud.buffer(Set(), Buffer.commitAsIs)

  protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]]

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Base[S <: Spores](
  final override val bud: S#Bud,
  knownDependencies: Set[Reactive[S]] = Set.empty[Reactive[S]]) extends  Reactive[S] {

  def staticIncoming: Set[Reactive[S]] = knownDependencies
}

class Reader[+P, S <: Spores](pulses: Buffer[Pulse[P]]) {
  def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get

  final def get(implicit turn: Turn[S]): P = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

/** A node that has nodes that depend on it */
trait Pulsing[+P, S <: Spores] extends Reactive[S] {
  protected[this] def strategy: (Pulse[P], Pulse[P]) => Pulse[P] = Buffer.transactionLocal[Pulse[P]]
  final protected[this] val pulses: S#TBuffer[Pulse[P]] = bud.buffer(Pulse.none, strategy)

  final def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get

  final val reader: Reader[P, S] = new Reader[P, S](pulses)
}

/** dynamic access to pulsing values */
trait PulseOption[+P, S <: Spores] extends Pulsing[P, S] {
  def apply(): Option[P] = throw new IllegalAccessException(s"$this.apply called outside of macro")
  final def apply[T](turn: Turn[S]): Option[P] = {
    turn.accessDynamic(this)
    turn.useDependency(this)
    pulse(turn).toOption
  }
}


/** a node that has a current state */
trait Stateful[+A, S <: Spores] extends Pulsing[A, S] {
  override protected[this] def strategy: (Pulse[A], Pulse[A]) => Pulse[A] = Buffer.keepPulse

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn[S]): A = {
    turn.accessDynamic(this)
    turn.useDependency(this)
    get(turn)
  }

  final def now(implicit maybe: Ticket[S]): A = maybe { t =>
    t.accessDynamic(this)
    get(t)
  }

  final def get(implicit turn: Turn[S]): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

