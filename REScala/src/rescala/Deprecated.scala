package rescala

import scala.collection.mutable.ListBuffer
import rescala.events._

/*
 * This file includes alternative implementations of Signal and Var which do not update
 * their dependencies during evaluation.
 * despite its name, it is very much required for the rest of the implementation.
 */

/** An implementation of Var with static dependencies */
class StaticVar[T](initval: T) extends Var[T] {
  private[this] var value: T = initval

  def set(newval: T): Unit = {
    val old = value
    if (newval != old) {
      value = newval // .asInstanceOf[T] // to make it covariant ?
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

  def update(v: T) = set(v)

  def reEvaluate(): T = value

  def map[B](f: T => B): Var[B] = StaticVar(f(get))
}
/**
 * Create a StaticVar
 */
object StaticVar {
  def apply[T](initval: T) = new StaticVar(initval)
}

/* A dependent reactive value which has static dependencies */
class StaticSignal[+T](reactivesDependsOn: List[DepHolder])(expr: => T) extends DependentSignal[T] {

  var inQueue = false

  private[this] var currentValue = expr

  def get = currentValue

  reactivesDependsOn.foreach(r => {
    if (r.level >= level) level = r.level + 1 // For glitch freedom
    r.addDependent(this) // To be notified in the future
  }) // check
  setDependOn(reactivesDependsOn)

  def triggerReevaluation() = reEvaluate()

  def reEvaluate(): T = {
    ReactiveEngine.log.nodeEvaluationStarted(this)
    inQueue = false
    val tmp = expr
    if (tmp != currentValue) {
      currentValue = tmp
      timestamps += TS.newTs // Testing
      notifyDependents(currentValue)
    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      timestamps += TS.newTs // Testing
    }
    ReactiveEngine.log.nodeEvaluationEnded(this)
    tmp
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (!inQueue) {
      inQueue = true
      ReactiveEngine.addToEvalQueue(this)
    }
  }

  def map[B](f: T => B): Signal[B] = StaticSignal(List(this))(f(this()))
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  def apply[T](reactivesDependsOn: List[DepHolder])(expr: => T) =
    new StaticSignal(reactivesDependsOn)(expr)

  type DH = DepHolder
  def apply[T]()(expr: => T): DependentSignal[T] = apply(List())(expr)
  def apply[T](r1: DH)(expr: => T): DependentSignal[T] = apply(List(r1))(expr)
  def apply[T](r1: DH, r2: DH)(expr: => T): DependentSignal[T] = apply(List(r1, r2))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH)(expr: => T): DependentSignal[T] = apply(List(r1, r2, r3))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH, r4: DH)(expr: => T): DependentSignal[T] = apply(List(r1, r2, r3, r4))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH, r4: DH, r5: DH)(expr: => T): DependentSignal[T] = apply(List(r1, r2, r3, r4, r5))(expr)
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
