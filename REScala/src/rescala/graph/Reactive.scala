package rescala.graph

import rescala.turns.Turn
import rescala.synchronization.TurnLock
import rescala.turns.Engine
import rescala.turns.Ticket
import rescala.graph.Pulse.{Diff, NoChange}
import scala.collection.immutable.Queue
import rescala.pipelining.Pipeline
/** A Reactive is something that can be reevaluated */
trait Reactive  {
  
  final override val hashCode: Int = Globals.nextID().hashCode()
  
  protected[rescala] def engine: Engine[Turn]
  protected[rescala] def lock: TurnLock
  private[rescala] final val pipeline = new Pipeline(this)
  
  private[rescala] final val level : Buffer[Int] = engine.bufferLevel(0, math.max, this)
  private[rescala] final val outgoing : Buffer[Set[Reactive]] = engine.bufferOutgoing(Set(), Buffer.commitAsIs, this)
  private[rescala] val incoming : Buffer[Set[Reactive]] = engine.bufferIncoming(Set(), Buffer.commitAsIs, this)
  
  
  

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
 
  protected[this] final val pulses : Buffer[Pulse[P]] = engine.bufferPulses(Pulse.none, Buffer.transactionLocal, this)
  
  final def pulse(implicit turn: Turn): Pulse[P] =  pulses.get
}


/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {

  pulses.initStrategy(Buffer.keepPulse)
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