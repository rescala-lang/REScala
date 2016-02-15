package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.propagation.Turn
import rescala.engines.Ticket

/** A Reactive is something that can be reevaluated */
trait Reactive[S <: Spores] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def bud: S#Struct[Reactive[S]]

  final protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = bud.incoming

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Base[P, S <: Spores](
  final override protected[this] val budP: S#StructP[P, Reactive[S]]) extends Pulsing[P, S]  {

  final override protected[rescala] def bud: S#Struct[Reactive[S]] = budP
}

/** A node that has nodes that depend on it */
trait Pulsing[+P, S <: Spores] extends Reactive[S] {
  protected[this] def budP: S#StructP[P, Reactive[S]]
  final protected[this] def pulses: Buffer[Pulse[P]] = budP.pulses
  final def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get
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

