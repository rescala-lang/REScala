package rescala.propagation

import rescala.propagation.Pulse.{Diff, NoChange}
import rescala.propagation.turns.{Commitable, Turn, TurnLock, TurnState}

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive {
  final private[propagation] val lock: TurnLock = new TurnLock(this)

  final private[propagation] val level: TurnState[Int] = TurnState(0, math.max)

  final private[propagation] val dependants: TurnState[Set[Reactive]] = TurnState(Set(), (_, x) => x)

  /** for testing */
  def getLevel(implicit maybe: Ticket) = maybe { level.get(_) }

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[propagation] def reevaluate()(implicit turn: Turn): EvaluationResult

  private val name = {
    val classname = getClass.getName
    val unqualifiedClassname = classname.substring(classname.lastIndexOf('.') + 1)

    val trace = Thread.currentThread().getStackTrace
    var i = 0
    while (trace(i).toString.startsWith("scala.") || trace(i).toString.startsWith("java.") ||
      (trace(i).toString.startsWith("rescala.") && !trace(i).toString.startsWith("rescala.test."))) i += 1

    s"${ trace(i).getFileName }(${ trace(i).getLineNumber })"
  }
  override def toString = name


}

/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
  final protected[this] val pulses: TurnState[Pulse[P]] = TurnState(Pulse.none, (x, _) => x)

  final def pulse(implicit turn: Turn): Pulse[P] = pulses.get
}

/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  pulses.commitStrategy = (_, p) => p.keep

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn): A = {
    DynamicsSupport.useDependency(this)
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
trait StaticReevaluation[+P] extends Pulsing[P] {
  /** side effect free calculation of the new pulse for the current turn */
  protected[propagation] def calculatePulse()(implicit turn: Turn): Pulse[P]

  final override protected[propagation] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val p = calculatePulse()
    pulses.set(p)
    EvaluationResult.Static(p.isChange)
  }
}

/** reevaluation strategy for dynamic dependencies */
trait DynamicReevaluation[+P] extends Pulsing[P] {
  private val dependencies: TurnState[Set[Reactive]] = TurnState(Set(), (_, x) => x)

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
