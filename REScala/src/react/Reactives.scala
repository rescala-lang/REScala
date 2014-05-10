package react

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.Queue
import scala.reflect.runtime.universe._
import react.events._
import react.log._
import react.log.Logging

/* A Reactive is a value type which has a dependency to other Reactives */
trait Reactive extends Ordered[Reactive] {
  // testing
  val timestamps: Buffer[Stamp] = ListBuffer()

  var level: Int = 0
  override def compare(other: Reactive): Int =
    other.level - this.level

  ReactiveEngine.log.nodeCreated(this)
}

object Reactive {
  /* Reactive are comparable by their level */
  implicit def ord[T <: Reactive]: Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = if (x == null) -1 else x compare y
  }
}

/* A node that has nodes that depend on it */
trait DepHolder extends Reactive {
  val dependents: Buffer[Dependent] = ListBuffer()
  def addDependent(dep: Dependent) = {
    dependents += dep
    ReactiveEngine.log.nodeAttached(dep, this)
  }
  def removeDependent(dep: Dependent) = dependents -= dep
  def notifyDependents(change: Any): Unit = {
    ReactiveEngine.log.nodePulsed(this)
    dependents.foreach(_.dependsOnchanged(change, this))
  }
}

/* A node that depends on other nodes */
trait Dependent extends Reactive {
  val dependOn: Buffer[DepHolder] = ListBuffer()
  // TODO: add level checking to have glitch freedom ?
  def addDependOn(dep: DepHolder) = {
    dependOn += dep
    ReactiveEngine.log.nodeAttached(this, dep)
  }
  def removeDependOn(dep: DepHolder) = dependOn -= dep

  protected[react] def triggerReevaluation()

  /* A node on which this one depends is changed */
  def dependsOnchanged(change: Any, dep: DepHolder)
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

trait ReactiveValue[+T] extends Changing[T] with DepHolder {
  def getValue: T

  def apply(): T

  def apply(s: SignalSynt[_]): T = {
    if (level >= s.level) s.level = level + 1
    s.reactivesDependsOnCurrent += this
    getValue
  }

  protected[react] def reEvaluate(): T
}

/* A root Reactive value without dependencies which can be set */
trait Var[T] extends ReactiveValue[T] {
  def setValue(newval: T): Unit
  def update(v: T)

  def toSignal: Signal[T]
}

object Var {
  def apply[T](initval: T) = new VarSynt(initval)
}

trait Foldable {
  /** folds with a given fold function to create a reactive value */
  def fold[T, A](e: Event[T], init: A)(f: (A, T) => A): ReactiveValue[A]

  /* ---------- derived methods follow ---------- */

  /** Iterates a value on the occurrence of the event. */
  def iterate[A](e: Event[_], init: A)(f: A => A): ReactiveValue[A] =
    fold(e, init)((acc, _) => f(acc))

  /**
    * Counts the occurrences of the event. Starts from 0, when the event has never been
    *  fired yet. The argument of the event is simply discarded.
    */
  def count(e: Event[_]): ReactiveValue[Int] = fold(e, 0)((acc, _) => acc + 1)

  /**
    * Calls f on each occurrence of event e, setting the Signal to the generated value.
    *  The initial signal is obtained by f(init)
    */
  def set[F, G](e: Event[F], init: F)(f: F => G): ReactiveValue[G] =
    fold(e, f(init))((_, v) => f(v))

  /** returns a signal holding the latest value of the event. */
  def latest[T](e: Event[T], init: T): ReactiveValue[T] =
    fold(e, init)((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  def latestOption[T](e: Event[T]): ReactiveValue[Option[T]] =
    latest(e.map((x: T) => Some(x)), None)

  /** collects events resulting in a variable holding a list of all values. */
  def list[T](e: Event[T]): ReactiveValue[Seq[T]] =
    fold(e, List[T]())((acc, v) => v :: acc)

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last[T](e: Event[T], n: Int): ReactiveValue[Seq[T]] =
    fold(e, Seq[T]()) { (acc,v) => acc.takeRight(n-1) :+ v }
}

/* An inner node which depends on other values */
trait Signal[+T]
    extends ReactiveValue[T]
    with Dependent
    with Foldable {

    /** folds with a given fold function to create a reactive value */
  override def fold[T, A](e: Event[T], init: A)(f: (A, T) => A): ReactiveValue[A] =
    new FoldedSignal(e, init, f)

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  def snapshot(e: Event[_]): Signal[T] = IFunctions.snapshot(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchTo[U >: T](e: Event[U]): Signal[U] = IFunctions.switchTo(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchOnce[V >: T](e: Event[_])(newSignal: Signal[V]): Signal[V] = IFunctions.switchOnce(e, this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  def toggle[V >: T](e: Event[_])(other: Signal[V]) = IFunctions.toggle(e, this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int): Signal[T] = IFunctions.delay(this, n)

}

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
object ReactiveEngine {

  /* If logging is needed, replace this with another instance of Logging */
  var log: Logging = NoLogging

  private var evalQueue = new PriorityQueue[Dependent]

  /* Adds a dependant to the eval queue, duplicates are allowed */
  def addToEvalQueue(dep: Dependent): Unit = {
    evalQueue.synchronized {
      //if (evalQueue.exists(_ eq dep)) return

      ReactiveEngine.log.nodeScheduled(dep)
      evalQueue += dep

      // DEBUG:
      //if(evalQueue.toList.contains((_: Any) == null))
      //  System.err.println("eval queue contains null element after insertion of " + dep)
    }
  }

  def removeFromEvalQueue(dep: Dependent) = evalQueue = evalQueue.filter(_ eq dep)

  /* Evaluates all the elements in the queue */
  def startEvaluation() = {
    evalQueue.synchronized {
      val localStamp = TS.getCurrentTs
      // DEBUG: println("Start eval: " + Thread.currentThread() + "  " + localStamp + " (init. queue: " + evalQueue.length + ")")
      var counter = 0
      while (!evalQueue.isEmpty) {
        counter += 1
        val head = evalQueue.dequeue
        if (head == null) {
          System.err.println("priority deque yielded null")
          // not sure why this happens, null is never inserted
        } else {
          log.nodeEvaluationStarted(head)
          head.triggerReevaluation
          log.nodeEvaluationEnded(head)
        }
      }
      // DEBUG: println("End eval: " + Thread.currentThread() + "  " + localStamp + " (" + counter + " rounds)")
    }
  }
}

// TODO: check the use of these classes. Originally was only for testing
sealed case class Stamp(roundNum: Int, sequenceNum: Int)

object TS {
  private var _roundNum = 0
  private var _sequenceNum = 0

  def nextRound() {
    _roundNum += 1
    _sequenceNum = 0

    ReactiveEngine.log.logRound(getCurrentTs)
  }

  def newTs = {
    val ts = new Stamp(_roundNum, _sequenceNum)
    _sequenceNum += 1
    ReactiveEngine.log.logRound(ts)
    ts
  }

  def getCurrentTs = new Stamp(_roundNum, _sequenceNum)

  def reset() {
    _roundNum = 0
    _sequenceNum = 0
  }
}
