package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.synchronization.TurnLock
import rescala.turns.{Engine, Ticket, Turn}

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive {
  final override val hashCode: Int = Globals.nextID()

  protected[rescala] def lock: TurnLock

  protected[rescala] def engine: Engine[Turn]

  final private[rescala] val level: Buffer[Int] = engine.buffer(0, math.max, lock)

  final private[rescala] val dependants: Buffer[Set[Reactive]] = engine.buffer(Set(), (_, x) => x, lock)

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}

/** helper class to initialise engine and select lock */
abstract class Enlock(final override protected[rescala] val engine: Engine[Turn], lockOverride: Set[Reactive] = Set()) extends Reactive {
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


/** reevaluation strategy for static dependencies */
trait StaticReevaluation[+P] {
  this: Pulsing[P] =>
  /** side effect free calculation of the new pulse for the current turn */
  protected[rescala] def calculatePulse()(implicit turn: Turn): Pulse[P]

  final override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val p = calculatePulse()
    pulses.set(p)
    EvaluationResult.Static(p.isChange)
  }
}

/** reevaluation strategy for dynamic dependencies */
trait DynamicReevaluation[+P] {
  this: Pulsing[P] =>

  private val dependencies: Buffer[Set[Reactive]] = engine.buffer(Set(), (_, x) => x, lock)

  /** side effect free calculation of the new pulse and the new dependencies for the current turn */
  def calculatePulseDependencies(implicit turn: Turn): (Pulse[P], Set[Reactive])

  final override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val (newPulse, newDependencies) = calculatePulseDependencies

    val oldDependencies = dependencies.get
    dependencies.set(newDependencies)
    pulses.set(newPulse)
    EvaluationResult.Dynamic(newPulse.isChange, DepDiff(newDependencies, oldDependencies))

  }
}
