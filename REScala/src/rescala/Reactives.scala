package rescala

import java.util.UUID

import rescala.events._
import rescala.log.ReactiveLogging
import rescala.propagation.{NoChangePulse, Pulse, Turn}
import rescala.signals.Signal

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive extends ReactiveLogging {

  val id: UUID = UUID.randomUUID()

  private var _level = 0
  def ensureLevel(l: Int): Unit = if (l >= _level) _level = l + 1
  def level: Int = _level

  log.nodeCreated(this)
}

/** A node that has nodes that depend on it */
trait Dependency[+P] extends Reactive {
  private var dependants: Set[Dependant] = Set()

  /** used for testing */
  def dependentCount(): Int = dependants.size

  def addDependant(dep: Dependant): Unit = {
    if (!dependants.contains(dep)) {
      dependants += dep
      log.nodeAttached(dep, this)
    }
  }

  def removeDependant(dep: Dependant) = dependants -= dep

  final def notifyDependants(implicit turn: Turn): Unit = {
    log.nodePulsed(this)
    dependants.foreach(_.dependencyChanged(this)(turn))
  }

  override def ensureLevel(l: Int): Unit = {
    val oldLevel = level
    super.ensureLevel(l)
    val newLevel = level
    if (oldLevel < newLevel) dependants.foreach(_.ensureLevel(newLevel))
  }

  private[this] var pulses: Map[Turn, Pulse[P]] = Map()

  def pulse(implicit turn: Turn): Pulse[P] = pulses.getOrElse(turn, NoChangePulse)
  protected[this] def pulse(pulse: Pulse[P])(implicit turn: Turn) = {
    pulses += turn -> pulse
    notifyDependants(turn)
  }

}

/** A node that depends on other nodes */
trait Dependant extends Reactive {
  private var dependencies: Set[Dependency[_]] = Set()

  /** for testing */
  def dependencyCount(): Int = dependencies.size

  def addDependency(dep: Dependency[_]): Unit = {
    if (!dependencies.contains(dep)) {
      ensureLevel(dep.level)
      dependencies += dep
      dep.addDependant(this)
    }
  }
  def setDependencies(deps: TraversableOnce[Dependency[_]]): Unit = {
    val newDependencies = deps.toSet
    val removed = dependencies.diff(newDependencies)
    val added = newDependencies.diff(dependencies)
    removed.foreach(removeDependency)
    added.foreach(addDependency)
    dependencies = deps.toSet
  }
  def removeDependency(dep: Dependency[_]): Unit = {
    dep.removeDependant(this)
    dependencies -= dep
  }

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def triggerReevaluation()(implicit turn: Turn): Unit

  /** callback when a dependency has changed */
  def dependencyChanged[Q](dep: Dependency[Q])(implicit turn: Turn): Unit = turn.addToEvalQueue(this)
}

trait Changing[+T] {
  this: Dependency[T] =>

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
trait DependentSignal[+T] extends Signal[T] with Dependant


