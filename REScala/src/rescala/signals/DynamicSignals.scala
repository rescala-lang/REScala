package rescala.signals

import rescala._
import rescala.propagation.{NoChangePulse, DiffPulse, Turn}


abstract class DependentSignalImplementation[+T](creationTurn: Turn) extends DependentSignal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateNewValue()(implicit turn: Turn): T

  {
    log.nodeEvaluationStarted(this)
    currentValue = initialValue()(creationTurn)
    log.nodeEvaluationEnded(this)
  }

  override def triggerReevaluation()(implicit turn: Turn): Unit = {
    log.nodeEvaluationStarted(this)

    val oldLevel = level

     // Evaluation
    val newValue = try {
      calculateNewValue()
    }
    catch {
      case e: Exception =>
        log.nodeEvaluationEndedWithException(this, e)
        throw e
    }

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
          pulse(DiffPulse(newValue, currentValue))
        }
        else {
          pulse(NoChangePulse)
          log.nodePropagationStopped(this)
        }
      } : Unit
    }
    log.nodeEvaluationEnded(this)
  }
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
