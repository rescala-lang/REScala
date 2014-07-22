package rescala

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.Queue
import scala.reflect.runtime.universe._
import rescala.events._
import rescala.log._
import rescala.log.Logging
import java.util.UUID

/* A Reactive is a value type which has a dependency to other Reactives */
trait Reactive extends Ordered[Reactive] {
  // testing
  val timestamps: Buffer[Stamp] = ListBuffer()
  
  var id = UUID.randomUUID()

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
    if (!dependents.contains(dep)) {
      dependents += dep
      ReactiveEngine.log.nodeAttached(dep, this)
    }
  }
  def addAllDependent(deps: TraversableOnce[Dependent]) = {
    for (dep <- deps) {
      addDependent(dep)
    }
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
    if (!dependOn.contains(dep)) {
      dependOn += dep
      ReactiveEngine.log.nodeAttached(this, dep)
    }
  }
  def setDependOn(deps: TraversableOnce[DepHolder]) = {
    dependOn.clear()
    dependOn ++= deps
  }
  def removeDependOn(dep: DepHolder) = dependOn -= dep

  protected[rescala] def triggerReevaluation(): Unit

  /* A node on which this one depends is changed */
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

trait Signal[+A] extends Changing[A] with FoldableReactive[A] with DepHolder {
  override def fold[B](init: B)(f: (B, A) => B): Signal[B] =
    new FoldedSignal(changed, init, f)

  def get: A

  def apply(): A

  def apply(s: SignalSynt[_]): A = {
    if (level >= s.level) s.level = level + 1
    s.reactivesDependsOnCurrent += this
    get
  }

  def map[B](f: A => B): Signal[B]

  protected[rescala] def reEvaluate(): A

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  def snapshot(e: Event[_]): Signal[A] = IFunctions.snapshot(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchTo[U >: A](e: Event[U]): Signal[U] = IFunctions.switchTo(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchOnce[V >: A](e: Event[_])(newSignal: Signal[V]): Signal[V] = IFunctions.switchOnce(e, this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  def toggle[V >: A](e: Event[_])(other: Signal[V]) = IFunctions.toggle(e, this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int): Signal[A] = IFunctions.delay(this, n)
  
  /** Unwraps a Signal[Event[E]] to an Event[E] */
  def unwrap[E](implicit evidence: A <:< Event[E]): Event[E] = IFunctions.unwrap(this.map(evidence))

}

/* A root Reactive value without dependencies which can be set */
trait Var[T] extends Signal[T] {
  def set(newval: T): Unit
  def update(v: T): Unit
}

object Var {
  def apply[T](initval: T) = new VarSynt(initval)
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
  def list: Signal[Seq[A]] =
    fold(List[A]())((acc, v) => v :: acc)

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last(n: Int): Signal[Seq[A]] =
    fold(Seq[A]()) { (acc,v) => acc.takeRight(n-1) :+ v }
}

/* An inner node which depends on other values */
trait DependentSignal[+T] extends Signal[T] with Dependent

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
      while (evalQueue.nonEmpty) {
        counter += 1
        val head = evalQueue.dequeue()
        if (head == null) {
          System.err.println("priority deque yielded null")
          // not sure why this happens, null is never inserted
        } else {
          log.nodeEvaluationStarted(head)
          head.triggerReevaluation()
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

  def nextRound(): Unit = {
    _roundNum += 1
    _sequenceNum = 0

    ReactiveEngine.log.logRound(getCurrentTs)
  }

  def newTs: Stamp = {
    val ts = new Stamp(_roundNum, _sequenceNum)
    _sequenceNum += 1
    ReactiveEngine.log.logRound(ts)
    ts
  }

  def getCurrentTs = new Stamp(_roundNum, _sequenceNum)

  def reset(): Unit = {
    _roundNum = 0
    _sequenceNum = 0
  }
}
