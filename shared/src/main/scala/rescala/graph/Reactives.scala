package rescala.graph

import rescala.engines.Ticket
import rescala.graph.Pulse.{Diff, NoChange}
import rescala.propagation.Turn

/**
  * A reactive value is something that can be reevaluated
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Reactive[S <: Struct] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  /**
    * Spore that is used to internally manage the reactive evaluation of this value
    *
    * @return Spore for this value
    */
  protected[rescala] def bud: S#Spore[Reactive[S]]

  /**
    * Reevaluates this value when it is internally scheduled for reevaluation
    *
    * @param turn Turn that handles the reevaluation
    * @return Result of the reevaluation
    */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Base[P, S <: Struct](budP: S#SporeP[P, Reactive[S]]) extends Pulsing[P, S] {
  final override protected[rescala] def bud: S#Spore[Reactive[S]] = budP
  final override protected[this] def pulses(implicit turn: Turn[S]): Buffer[Pulse[P]] = turn.pulses(budP)

  final override def pulse(implicit turn: Turn[S]): Pulse[P] = pulses.get
}

/**
  * A pulsing value is a reactive value that stores a pulse with it's old and new value
  *
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Pulsing[+P, S <: Struct] extends Reactive[S] {
  protected[this] def pulses(implicit turn: Turn[S]): Buffer[Pulse[P]]
  def pulse(implicit turn: Turn[S]): Pulse[P]
}

/**
  * A reactive value that may have a current state that can be read.
  *
  * @tparam P Value type stored by the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait PulseOption[+P, S <: Struct] extends Pulsing[P, S] {
  def apply(): Option[P] = throw new IllegalAccessException(s"$this.apply called outside of macro")
  final def apply[T](turn: Turn[S]): Option[P] = {
    turn.dependencyInteraction(this)
    turn.useDependency(this)
    pulse(turn).toOption
  }
}


/**
  * A reactive value that has a current state that can be read
  *
  * @tparam A Value type stored by the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
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

