package rescala

import scala.collection.mutable.ListBuffer
import rescala.events._

/*
 * This file includes alternative implementations of Signal and Var which do not update
 * their dependencies during evaluation.
 * despite its name, it is very much required for the rest of the implementation.
 */

/** An implementation of Var with static dependencies */
class StaticVar[T](private[this] var value: T) extends Var[T] {

  def set(newValue: T): Unit = {
    if (newValue != value) {
      value = newValue // .asInstanceOf[T] // to make it covariant ?
      TS.nextRound() // Testing
      timestamps += TS.newTs // testing

      notifyDependents(value)
      ReactiveEngine.startEvaluation()
    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      timestamps += TS.newTs // testing
    }
  }

  def get = value

  def reEvaluate(): T = value
}

/**
 * Create a StaticVar
 */
object StaticVar {
  def apply[T](initialValue: T) = new StaticVar(initialValue)
}

/** A dependent reactive value which has static dependencies */
class StaticSignal[+T](reactivesDependsOn: List[DepHolder])(expr: => T) extends DependentSignal[T] {

  var inQueue = false

  private[this] var currentValue = expr

  def get = currentValue

  reactivesDependsOn.foreach(r => {
    if (r.level >= level) level = r.level + 1 // For glitch freedom
    r.addDependent(this) // To be notified in the future
  }) // check
  setDependOn(reactivesDependsOn)

  protected[rescala] def triggerReevaluation() = reEvaluate()

  def reEvaluate(): T = {
    ReactiveEngine.log.nodeEvaluationStarted(this)
    inQueue = false
    val newValue = expr
    if (newValue != currentValue) {
      currentValue = newValue
      timestamps += TS.newTs // Testing
      notifyDependents(currentValue)
    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      timestamps += TS.newTs // Testing
    }
    ReactiveEngine.log.nodeEvaluationEnded(this)
    newValue
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (!inQueue) {
      inQueue = true
      ReactiveEngine.addToEvalQueue(this)
    }
  }
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  def apply[T](reactivesDependsOn: List[DepHolder])(expr: => T) =
    new StaticSignal(reactivesDependsOn)(expr)

  def apply[T]()(expr: => T): DependentSignal[T] = apply(List())(expr)
  def apply[T](dependencyHolders: DepHolder*)(expr: => T): DependentSignal[T] = apply(dependencyHolders.toList)(expr)
}

/* TODO: Do we really need two types of handlers? Can we use EventHandler? */
object Handler {
  //def apply[T] (exp: => T) = new EventHandler((_: Unit) => exp)
  def apply[T](exp: => T) = new Handler(exp)
}

class Handler[T](exp: => T) extends Dependent {
  override def dependsOnchanged(change: Any, dep: DepHolder) = exp
  def triggerReevaluation() = exp
}
