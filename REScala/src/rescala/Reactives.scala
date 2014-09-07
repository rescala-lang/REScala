package rescala

import rescala.events._
import rescala.log.ReactiveLogging
import rescala.signals.Signal

import scala.collection.mutable

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive extends ReactiveLogging {
  var _level = 0
  def ensureLevel(l: Int): Unit = if (l >= _level) _level = l + 1
  def level: Int = _level

  log.nodeCreated(this)
}

/** A node that has nodes that depend on it */
trait DepHolder extends Reactive {
  private var dependents: Set[Dependent] = Set()

  /** used for testing*/
  def dependentCount() = dependents.size

  def addDependent(dep: Dependent) = {
    if (!dependents.contains(dep)) {
      dependents += dep
      log.nodeAttached(dep, this)
    }
  }
  def removeDependent(dep: Dependent) = dependents -= dep
  def notifyDependents(change: Any): Unit = {
    log.nodePulsed(this)
    dependents.foreach(_.dependsOnchanged(change, this))
  }

  override def ensureLevel(l: Int): Unit = {
    val oldLevel = level
    super.ensureLevel(l)
    val newLevel = level
    if (oldLevel < newLevel) dependents.foreach(_.ensureLevel(newLevel))
  }
}

/** A node that depends on other nodes */
trait Dependent extends Reactive {
  private var dependOn: Set[DepHolder] = Set()

  /** for testing */
  def dependOnCount() = dependOn.size

  def addDependOn(dep: DepHolder) = {
    if (!dependOn.contains(dep)) {
      ensureLevel(dep.level)
      dependOn += dep
      dep.addDependent(this)
      log.nodeAttached(this, dep)
    }
  }
  def setDependOn(deps: TraversableOnce[DepHolder]) = {
    val newDependencies = deps.toSet
    val removed = dependOn.diff(newDependencies)
    val added = newDependencies.diff(dependOn)
    removed.foreach(removeDependOn)
    added.foreach(addDependOn)
    dependOn = deps.toSet
  }
  def removeDependOn(dep: DepHolder) = {
    dep.removeDependent(this)
    dependOn -= dep
  }

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def triggerReevaluation(): Unit

  /** callback when a dependency has changed */
  def dependsOnchanged(change: Any, dep: DepHolder): Unit
}

trait Changing[+T] {
  this: DepHolder =>

  /**
   * Create an event that fires every time the signal changes. It fires the tuple
   *  (oldVal, newVal) for the signal. The first tuple is (null, newVal)
   */
  lazy val change: Event[(T, T)] = new ChangedEventNode(this)

  /**
   * Create an event that fires every time the signal changes. The value associated
   * to the event is the new value of the signal
   */
  lazy val changed: Event[T] = change map ((x: (T, T)) => { x._2 })

    /** Convenience function filtering to events which change this reactive to value */
  def changedTo[V](value: V): Event[Unit] = (changed && { _ == value }).dropParam
}


trait FoldableReactive[+A] {
  def fold[B](init: B)(f: (B, A) => B): Signal[B]

  /* ---------- derived methods follow ---------- */

  /** Iterates a value on the occurrence of the event. */
  def iterate[B](init: B)(f: B => B): Signal[B] =
    fold(init)((acc, _) => f(acc))

  /**
    * Counts the occurrences of the event. Starts from 0, when the event has never been
    *  fired yet. The argument of the event is simply discarded.
    */
  def count: Signal[Int] = fold(0)((acc, _) => acc + 1)

  /** Holds the latest value of an event as an Option, None before the
    * first event occured */
  def latestOption: Signal[Option[A]] =
    fold(None: Option[A])((acc, v) => Some(v))

  /** collects events resulting in a variable holding a list of all values. */
  def list: Signal[List[A]] =
    fold(List[A]())((acc, v) => v :: acc)

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last(n: Int): Signal[Seq[A]] =
    fold(Seq[A]()) { (acc,v) => acc.takeRight(n-1) :+ v }
}


/** An inner node which depends on other values */
trait DependentSignal[+T] extends Signal[T] with Dependent


