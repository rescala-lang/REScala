package rescala

import scala.collection.mutable.ListBuffer
import rescala.events.Event
import rescala.events.ChangedEventNode
import rescala.events.EventNode

/* A node that has nodes that depend on it */
class VarSynt[T](private[this] var value: T) extends Var[T] {

  def get = value

  def set(newValue: T): Unit = ReactiveEngine.synchronized {
    if (value != newValue) {
      value = newValue

      notifyDependents(value)
      ReactiveEngine.startEvaluation()

    } else {
      log.nodePropagationStopped(this)
    }
  }

  def reEvaluate(): T = value
}

object VarSynt {
  def apply[T](initialValue: T) = new VarSynt(initialValue)
}

trait DependentSignalImplementation[+T] extends DependentSignal[T] {

  def initialValue(): T
  def calculateNewValue(): T

  private[this] var currentValue = initialValue()

  def get = currentValue

  def triggerReevaluation(): Unit = {
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
      ReactiveEngine.addToEvalQueue(this)
    }
    else {
      if (level <= oldLevel) {
        /* Notify dependents only of the value changed */
        if (currentValue != newValue) {
          currentValue = newValue
          notifyDependents(currentValue)
        }
        else {
          log.nodePropagationStopped(this)
        }
      } : Unit
    }
    log.nodeEvaluationEnded(this)
  }
  override def dependsOnchanged(change: Any, dep: DepHolder) = ReactiveEngine.addToEvalQueue(this)

}

/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class SignalSynt[+T](reactivesDependsOnUpperBound: List[DepHolder])(expr: SignalSynt[T] => T)
  extends { private var detectedDependencies = Set[DepHolder]() } with DependentSignalImplementation[T] {

  override def onDynamicDependencyUse[A](dependency: Signal[A]): Unit = {
    super.onDynamicDependencyUse(dependency)
    detectedDependencies += dependency
  }

  override def initialValue(): T = calculateNewValue()

  override def calculateNewValue(): T = {
    val newValue = expr(this)
    setDependOn(detectedDependencies)
    detectedDependencies = Set()
    newValue
  }

  if(reactivesDependsOnUpperBound.nonEmpty) ensureLevel(reactivesDependsOnUpperBound.map{_.level}.max)

}

/**
 * A syntactic signal
 */
object SignalSynt {
  def apply[T](reactivesDependsOn: List[DepHolder])(expr: SignalSynt[T] => T) =
    new SignalSynt(reactivesDependsOn)(expr)

  def apply[T](expr: SignalSynt[T] => T): SignalSynt[T] = apply(List())(expr)
  def apply[T](dependencyHolders: DepHolder*)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(dependencyHolders.toList)(expr)

}






