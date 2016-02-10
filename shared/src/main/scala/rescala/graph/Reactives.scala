package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.turns.{Ticket, Turn}

/** A Reactive is something that can be reevaluated */
trait Reactive[S <: Spores] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def bud[P]: S#Bud[P]

  protected[rescala] def outgoing(implicit turn: Turn[S]): Set[Reactive[S]] = bud.outgoing.asInstanceOf[Set[Reactive[S]]]

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
  final val _bud: S#Bud[_],
  knownDependencies: Set[Reactive[S]] = Set.empty[Reactive[S]]) extends Reactive[S] {

  final override protected[rescala] def bud[Q]: S#Bud[Q] = _bud.asInstanceOf[S#Bud[Q]]

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
  final protected[this] def pulses: Buffer[Pulse[P]] = bud.pulses
  final def pulse(implicit turn: Turn[S]): Pulse[P] = bud.pulses.get

  final val reader: Reader[P, S] = new Reader[P, S](bud.pulses)
}

/** dynamic access to pulsing values */
trait PulseOption[+P, S <: Spores] extends Pulsing[P, S] {
  def apply(): Option[P] = throw new IllegalAccessException(s"$this.apply called outside of macro")
  final def apply[T](turn: Turn[S]): Option[P] = {
    turn.dependencyInteraction(this)
    turn.useDependency(this)
    pulse(turn).toOption
  }
}


/** a node that has a current state */
trait Stateful[+A, S <: Spores] extends Pulsing[A, S] {
  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn[S]): A = {
    turn.dependencyInteraction(this)
    turn.useDependency(this)
    get(turn)
  }

  final def now(implicit maybe: Ticket[S]): A = maybe { t =>
    t.dependencyInteraction(this)
    get(t)
  }

  final def get(implicit turn: Turn[S]): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

