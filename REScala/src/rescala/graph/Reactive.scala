package rescala.graph

import rescala.turns.Turn
import rescala.synchronization.TurnLock
import rescala.turns.Engine
import rescala.turns.Ticket
import rescala.graph.Pulse.{Diff, NoChange}
import scala.collection.immutable.Queue

/** A Reactive is something that can be reevaluated */
trait Reactive extends Framed {
  
  protected[this] override type D <: ReactiveTurnData
  
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def engine: Engine[Turn]
  
   protected[rescala] def lock: TurnLock 

  final private[rescala] def level (implicit turn: Turn) = frame(_.level)
  final private[rescala] def outgoing (implicit turn: Turn) = frame(_.outgoing)
  protected[rescala] def incoming(implicit turn: Turn) = frame(_.incoming)

  

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
  protected [this] override type D <:PulsingTurnData[P]
  final def pulse(implicit turn: Turn): Pulse[P] = frame(_.pulses).get
  protected[this] def pulses(implicit turn:Turn): Buffer[Pulse[P]] = frame(_.pulses)
}


/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  protected [this] override type D <:StatefulTurnData[A]

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn): A = {
    turn.accessDynamic(this)
    Globals.useDependency(this)
    get(turn)
  }

  final def now(implicit maybe: Ticket): A = maybe { get(_) }

  final def get(implicit turn: Turn): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}