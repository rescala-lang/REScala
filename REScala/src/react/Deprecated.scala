package react

import scala.collection.mutable.ListBuffer
import react.events._

/**
 * This file includes alternative implementations of Signal and Var which do not update
 *  their dependencies during evaluation.
 */

/* An implementation of Var with static dependencies */
class StaticVar[T](initval: T) extends DepHolder with Var[T] {
  private[this] var value: T = initval

  def setValue(newval: T): Unit = {
    val old = value
    if (newval != old) {
      value = newval // .asInstanceOf[T] // to make it covariant ?
      TS.nextRound // Testing
      timestamps += TS.newTs // testing

      notifyDependents(value)
      ReactiveEngine.startEvaluation
    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      timestamps += TS.newTs // testing
    }
  }

  def getValue = value

  def update(v: T) = setValue(v)

  def apply = getValue

  def toSignal = StaticSignal(this) { this.getValue }

  def reEvaluate(): T = value
}
/**
 * Create a StaticVar
 */
object StaticVar {
  def apply[T](initval: T) = new StaticVar(initval)
}

/* A dependent reactive value which has static dependencies */
class StaticSignal[+T](reactivesDependsOn: List[DepHolder])(expr: => T)
  extends Dependent with DepHolder with Signal[T] {

  var inQueue = false

  private[this] var currentValue = expr

  def getValue = currentValue
  def getVal = currentValue

  def apply(): T = currentValue

  reactivesDependsOn.foreach(r => {
    if (r.level >= level) level = r.level + 1 // For glitch freedom
    r.addDependent(this) // To be notified in the future
  }) // check
  dependOn ++= reactivesDependsOn

  def triggerReevaluation() = reEvaluate

  def reEvaluate(): T = {
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
    tmp
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

  type DH = DepHolder
  def apply[T]()(expr: => T): Signal[T] = apply(List())(expr)
  def apply[T](r1: DH)(expr: => T): Signal[T] = apply(List(r1))(expr)
  def apply[T](r1: DH, r2: DH)(expr: => T): Signal[T] = apply(List(r1, r2))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH)(expr: => T): Signal[T] = apply(List(r1, r2, r3))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH, r4: DH)(expr: => T): Signal[T] = apply(List(r1, r2, r3, r4))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH, r4: DH, r5: DH)(expr: => T): Signal[T] = apply(List(r1, r2, r3, r4, r5))(expr)
}

/* TODO: Do we really need two types of handlers? Can we use EventHandler? */
object Handler {
  //def apply[T] (exp: => T) = new EventHandler((_: Unit) => exp)
  def apply[T](exp: => T) = new Handler(exp)
}

class Handler[T](exp: => T) extends Dependent {
  override def dependsOnchanged(change: Any, dep: DepHolder) = exp
  def triggerReevaluation = exp
}
