package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.synchronization.TurnLock
import rescala.turns.{Engine, Ticket, Turn}

/** A Reactive is something that can be reevaluated */
trait Reactive {
  final override val hashCode: Int = Globals.nextID()

  protected[rescala] def lock: TurnLock

  protected[rescala] def engine: Engine[Turn]

  final private[rescala] val level: Buffer[Int] = engine.buffer(0, math.max, lock)

  final private[rescala] val outgoing: Buffer[Set[Reactive]] = engine.buffer(Set(), (_, x) => x, lock)

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Enlock(final override protected[rescala] val engine: Engine[Turn],
                      lockOverride: Set[Reactive] = Set()) extends Reactive {
  final override protected[rescala] val lock: TurnLock =
    if (lockOverride.size == 1) lockOverride.head.lock
    else new TurnLock(this)
}


/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
  final protected[this] val pulses: Buffer[Pulse[P]] = engine.buffer(Pulse.none, (x, _) => x, lock)

  final def pulse(implicit turn: Turn): Pulse[P] = pulses.get
}


/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  pulses.initStrategy((_, p) => p.keep)

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

