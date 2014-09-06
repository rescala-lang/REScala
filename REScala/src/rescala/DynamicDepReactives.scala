package rescala

import scala.collection.mutable.ListBuffer
import rescala.events.Event
import rescala.events.ChangedEventNode
import rescala.events.EventNode

//trait FixedDepHolder extends Reactive {
//  val fixedDependents = new ListBuffer[Dependent]
//  def addFixedDependent(dep: Dependent) = fixedDependents += dep
//  def removeFixedDependent(dep: Dependent) = fixedDependents -= dep
// def notifyDependents(change: Any): Unit = dependents.map(_.dependsOnchanged(change,this))
//}

/* A node that has nodes that depend on it */
class VarSynt[T](private[this] var value: T) extends Var[T] {

  def get = value

  def set(newValue: T): Unit = ReactiveEngine.synchronized {
    if (value != newValue) {
      value = newValue
      TS.nextRound() // Testing
      logTestingTimestamp()

      notifyDependents(value)
      ReactiveEngine.startEvaluation()

    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      logTestingTimestamp() // testing
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
    ReactiveEngine.log.nodeEvaluationStarted(this)

    logTestingTimestamp() // Testing

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
          ReactiveEngine.log.nodePropagationStopped(this)
        }
      } : Unit
    }
    ReactiveEngine.log.nodeEvaluationEnded(this)
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





/** A wrapped event inside a signal, that gets "flattened" to a plain event node */
class WrappedEvent[T](wrapper: Signal[Event[T]]) extends EventNode[T] with Dependent {
  
  var currentValue: T = _
  
  updateDependencies()

  private def updateDependencies() = setDependOn(Set(wrapper, wrapper.get))

  def triggerReevaluation() {
    logTestingTimestamp()
    notifyDependents(currentValue)
  }
  
  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if(dep eq wrapper) {
	    updateDependencies()
    }
    else if(dep eq wrapper.get) {
      currentValue = change.asInstanceOf[T]
    	ReactiveEngine.addToEvalQueue(this)
    }
    else throw new IllegalStateException("Illegal DepHolder " + dep)

  }
  
}
