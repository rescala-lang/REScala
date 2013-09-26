package react


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.reflect.runtime.universe._
import react.events._

/* A Reactive is a value type which has a dependency to other Reactives */
trait Reactive extends Ordered[Reactive] {
  var level: Int = 0
  override def compare(other: Reactive): Int = 
    other.level - this.level
}

object Reactive {
  /* Reactive are comparable by their level */
  implicit def ord[T <: Reactive]: Ordering[T] = new Ordering[T] { 
    def compare(x: T, y: T) = x.compare(y)
  }
}


/* A node that has nodes that depend on it */
trait DepHolder extends Reactive {
  val dependents = new ListBuffer[Dependent]
  def addDependent(dep: Dependent) = dependents += dep    
  def removeDependent(dep: Dependent) = dependents -= dep
  def notifyDependents(change: Any): Unit = dependents.foreach(_.dependsOnchanged(change,this))  
}

/* A node that depends on other nodes */
trait Dependent extends Reactive {
  val dependOn = new ListBuffer[DepHolder]
  // TODO: add level checking to have glitch freedom ?
  def addDependOn(dep: DepHolder) = dependOn += dep    
  def removeDependOn(dep: DepHolder) = dependOn -= dep
  
  def triggerReevaluation() 
  
  /* A node on which this one depends is changed */
  def dependsOnchanged(change:Any, dep: DepHolder)
}


/* A root Reactive value without dependencies which can be set */
trait Var[T] extends DepHolder {
  def setVal(newval: T): Unit 
  def getValue: T
  def getVal: T
  def update(v: T)
  
  def apply(): T
  def apply(s: SignalSynt[_]): T
  
  def toSignal: Signal[T]
  
  /* Testing */
  val timestamps: ListBuffer[Stamp]
}


object Var {
  def apply[T](initval: T) = new VarSynt(initval)
}


/* An inner node which depends on other values */
trait Signal[+T] extends Dependent with DepHolder {
  
  def getValue: T
  def getVal: T
  
  def triggerReevaluation()
  
  def reEvaluate(): T 
  
  def apply(): T
  def apply(s: SignalSynt[_]): T

  /**
   * Create an event that fires every time the signal changes. It fires the tuple
   *  (oldVal, newVal) for the signal. The first tuple is (null, newVal)
   */
  def change[U >: T]: Event[(U, U)]
  
  /**
   * Create an event that fires every time the signal changes. The value associated
   * to the event is the new value of the signal
   */
  def changed[U >: T]: Event[U] = change map ((x: (U,U))=>{ x._2 })

  /** Convenience function filtering to events which change this reactive to value */
  def changedTo[V](value: V): Event[Unit] = change && { _._2 == value } dropParam
  
  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  def snapshot(e : Event[_]): Signal[T] = IFunctions.snapshot(e,this)

  /** Switch to (and keep) event value on occurrence of e*/  // TODO: check types
  def switchTo[U >: T](e : Event[U]): Signal[U] = IFunctions.switchTo(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchOnce[V >: T](e : Event[_])(newSignal : Signal[V]): Signal[V] = IFunctions.switchOnce(e, this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  def toggle[V >: T](e: Event[_])(other: Signal[V]) = IFunctions.toggle(e, this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int) = IFunctions.delay(this, n)

  
  /* Testing */
  val timestamps :ListBuffer[Stamp]
}



/**
 * Then engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph. 
 */
object ReactiveEngine {
  
  var evalQueue = new PriorityQueue[Dependent]

  /* Adds a dependant to the eval queue, duplicates are allowed */
  def addToEvalQueue(dep: Dependent): Unit = {
    //if (evalQueue.exists(_ eq dep)) return
    evalQueue += dep
  }
  
  def removeFromEvalQueue(dep: Dependent) = evalQueue = evalQueue.filter(_ eq dep)
  
  /* Evaluates all the elements in the queue */
  def startEvaluation = {
    this.synchronized {
    while (!evalQueue.isEmpty) {
      val head = evalQueue.dequeue
      head.triggerReevaluation
    }
    }
  }
}


// TODO: check the use of these classes. Originally was only for testing
sealed case class Stamp(roundNum: Int, sequenceNum: Int)

object TS {
  private var _roundNum = 0
  private var _sequenceNum = 0
  
  def nextRound {
    _roundNum += 1
    _sequenceNum = 0
  }
  
  def newTs = {
    val ts = new Stamp(_roundNum,_sequenceNum)
    _sequenceNum += 1
    ts
  } 
  
  def getCurrentTs = new Stamp(_roundNum,_sequenceNum)
  
  def reset {
    _roundNum = 0
    _sequenceNum = 0
  }
}
