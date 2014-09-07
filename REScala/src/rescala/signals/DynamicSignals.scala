package rescala.signals

import rescala._
import rescala.propagation.{NoChangePulse, DiffPulse, ValuePulse, Turn}

/* A node that has nodes that depend on it */
class VarSynt[T](private[this] var value: T) extends Var[T] {

  def get = value

  def set(newValue: T): Unit = Turn.newTurn { turn =>
    if (value != newValue) {
      turn.pulse(this, DiffPulse(newValue, value))
      turn.startEvaluation()
    } else {
      log.nodePropagationStopped(this)
    }
  }
}

object VarSynt {
  def apply[T](initialValue: T) = new VarSynt(initialValue)
}

abstract class DependentSignalImplementation[+T](creationTurn: Turn) extends DependentSignal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateNewValue()(implicit turn: Turn): T

  private[this] var currentValue = initialValue()(creationTurn)

  def get = currentValue

  override def triggerReevaluation()(implicit turn: Turn): Unit = {
    log.nodeEvaluationStarted(this)

    val oldLevel = level

     // Evaluation
    val newValue = calculateNewValue()

    /* if the level increases by one, the dependencies might or might not have been evaluated this turn.
     * if they have, we could just fire the observers, but if they have not we are not allowed to do so
     *
     * if the level increases by more than one, we depend on something that still has to be in the queue
     */
    if (level == oldLevel + 1) {
      turn.addToEvalQueue(this)
    }
    else {
      if (level <= oldLevel) {
        /* Notify dependents only of the value changed */
        if (currentValue != newValue) {
          turn.pulse(this, DiffPulse(newValue, currentValue))
        }
        else {
          turn.pulse(this, NoChangePulse)
          log.nodePropagationStopped(this)
        }
      } : Unit
    }
    log.nodeEvaluationEnded(this)
  }
  override def dependencyChanged[Q](dep: Dependency[Q])(implicit turn: Turn): Unit = turn.addToEvalQueue(this)

}

/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class SignalSynt[+T](
    dependenciesUpperBound: List[Dependency[Any]],
    private var detectedDependencies: Set[Dependency[Any]] = Set())
    (expr: SignalSynt[T] => T)
    (creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) {

  override def onDynamicDependencyUse[A](dependency: Signal[A]): Unit = {
    super.onDynamicDependencyUse(dependency)
    detectedDependencies += dependency
  }

  override def initialValue()(implicit turn: Turn): T = calculateNewValue()

  override def calculateNewValue()(implicit turn: Turn): T = {
    val newValue = expr(this)
    setDependencies(detectedDependencies)
    detectedDependencies = Set()
    newValue
  }

  if(dependenciesUpperBound.nonEmpty) ensureLevel(dependenciesUpperBound.map{_.level}.max)

}

/**
 * A syntactic signal
 */
object SignalSynt {
  def apply[T](dependencies: List[Dependency[Any]])(expr: SignalSynt[T] => T) = Turn.maybeTurn { turn =>
    new SignalSynt(dependencies)(expr)(turn)
  }

  def apply[T](expr: SignalSynt[T] => T): SignalSynt[T] = apply(List())(expr)
  def apply[T](dependencyHolders: Dependency[Any]*)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(dependencyHolders.toList)(expr)

}
