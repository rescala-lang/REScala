package rescala.graph

import rescala.engines.Ticket
import rescala.graph.Pulse.{Diff, NoChange}
import rescala.propagation.Turn

/** A Reactive is something that can be reevaluated */
trait Reactive[S <: Struct] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def bud: S#Spore[Reactive[S]]

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Base[P, S <: Struct](budP: S#SporeP[P, Reactive[S]]) extends Pulsing[P, S] {
  final override protected[rescala] def bud: S#Spore[Reactive[S]] = budP
  final override protected[this] def pulses: Buffer[Pulse[P]] = budP.pulses

  final override def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get
}

/** A node that has nodes that depend on it */
trait Pulsing[+P, S <: Struct] extends Reactive[S] {
  protected[this] def pulses: Buffer[Pulse[P]]
  def pulse(implicit turn: Turn[S]): Pulse[P]
}

/** dynamic access to pulsing values */
trait PulseOption[+P, S <: Struct] extends Pulsing[P, S] {
  def apply(): Option[P] = throw new IllegalAccessException(s"$this.apply called outside of macro")
  final def apply[T](turn: Turn[S]): Option[P] = {
    turn.dependencyInteraction(this)
    turn.useDependency(this)
    pulse(turn).toOption
  }
}


/** a node that has a current state */
trait Stateful[+A, S <: Struct] extends Pulsing[A, S] {
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

