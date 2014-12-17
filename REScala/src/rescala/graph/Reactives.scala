package rescala.graph

import rescala.turns.{Engine, Turn, Ticket}
import rescala.graph.Pulse.{Diff, NoChange}
import rescala.synchronization.TurnLock

/** A Reactive is a value type which has a dependency to other Reactives */
abstract class Reactive(val engine: Engine[Turn]) {
  final override val hashCode: Int = Globals.nextID()

  final private[rescala] val lock: TurnLock = new TurnLock(this)

  final private[rescala] val level: Buffer[Int] = engine.buffer(0, math.max, lock)

  final private[rescala] val dependants: Buffer[Set[Reactive]] = engine.buffer(Set(), (_, x) => x, null)

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}

/** A node that has nodes that depend on it */
abstract class Pulsing[+P](engine: Engine[Turn]) extends Reactive(engine) {
  final protected[this] val pulses: Buffer[Pulse[P]] = engine.buffer(Pulse.none, (x, _) => x, lock)

  final def pulse(implicit turn: Turn): Pulse[P] = pulses.get
}

/** a node that has a current state */
abstract class  Stateful[+A](engine: Engine[Turn]) extends Pulsing[A](engine) {
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
