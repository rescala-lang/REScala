package react


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.reflect.runtime.universe._
import react.events._


trait Reactive extends Ordered[Reactive] {
  var level: Int = 0
  override def compare(other: Reactive): Int = 
    other.level - this.level
}
object Reactive {
  /* All subclasses of Reactive are now comparable */
  implicit def ord[T <: Reactive]: Ordering[T] = new Ordering[T] { 
    def compare(x: T, y: T) = x.compare(y)
  }
}


/* A node that has nodes that depend on it */
trait DepHolder extends Reactive {
  val dependents = new ListBuffer[Dependent]
  def addDependent(dep: Dependent) = dependents += dep    
  def removeDependent(dep: Dependent) = dependents -= dep
  def notifyDependents(change: Any): Unit = dependents.map(_.dependsOnchanged(change,this)) 
}

/* A node that depends on other nodes */
trait Dependent extends Reactive {
  val dependOn = new ListBuffer[DepHolder]
  // TODO: add level checking to have glitch freedom ?
  def addDependOn(dep: DepHolder) = dependOn += dep    
  def removeDependOn(dep: DepHolder) = dependOn -= dep
  
  def triggerReevaluation() 
  
  /* A node on which this one depends is changed */
  def dependsOnchanged(change:Any,dep: DepHolder)
}



trait Var[T] extends DepHolder {
  def setVal(newval: T): Unit 
  def getValue: T
  def getVal: T
  def update(v: T)
  
  def apply(): T
  def apply(s: SignalSynt[_]): T
  
  /* Testing */
  val timestamps: ListBuffer[Stamp]
}


class StaticVar[T](initval: T) extends DepHolder with Var[T] {
  private[this] var value: T = initval
  def setVal(newval: T): Unit = {
    value = newval // .asInstanceOf[T] // to make it covariant ?
    
    TS.nextRound  // Testing
    timestamps += TS.newTs // testing

    notifyDependents(value)
    ReactiveEngine.startEvaluation
  }  
  def getValue = value
  def getVal = value
  
  def update(v: T) = setVal(v)
  
  def apply(s: SignalSynt[_]) = {
    if (level >= s.level) s.level = level + 1
    s.reactivesDependsOnCurrent += this 
    getVal
  }
  
  def apply = getVal
  
  /* Testing */
  val timestamps = ListBuffer[Stamp]()
}
object Var {
  def apply[T](initval: T) = new StaticVar(initval)
}




trait Signal[+T] extends Dependent with DepHolder {
  
  def getValue: T
  def getVal: T
  
  def triggerReevaluation()
  
  def reEvaluate(): T 
 
  /* To add handlers */
  def +=(handler: Dependent) 
  def -=(handler: Dependent)

  
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
  def toggle[V](e: Event[_])(other: Signal[V]) = IFunctions.toggle(e, this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int) = IFunctions.delay(change, this.getValue, n)

  
  /* Testing */
  val timestamps :ListBuffer[Stamp]
}


/**
 * A time changing value
 */
class StaticSignal[+T](reactivesDependsOn: List[DepHolder])(expr: =>T)
  extends Dependent with DepHolder with Signal[T] {

  private[this] var currentValue = expr
  
  def getValue = currentValue
  def getVal = currentValue
  
  def apply(): T = currentValue
  def apply(s: SignalSynt[_]) = {
    if (level >= s.level) s.level = level + 1
    s.reactivesDependsOnCurrent += this 
    getVal
  }
  
  reactivesDependsOn.map( r => {
    if (r.level >= level) level = r.level + 1 // For glitch freedom  
    r.addDependent(this) // To be notified in the future
  }) // check
  dependOn ++= reactivesDependsOn
  
  
  def triggerReevaluation() = reEvaluate
  
  def reEvaluate(): T = {
    val tmp = expr
    if (tmp != currentValue) {
      currentValue = tmp
      timestamps += TS.newTs // Testing
      notifyDependents(currentValue)
    } else {
      timestamps += TS.newTs // Testing
    }
    tmp
  }
  override def dependsOnchanged(change: Any,dep: DepHolder) = {
    ReactiveEngine.addToEvalQueue(this)
  }
  
  /* To add handlers */
  def +=(handler: Dependent) {
    handler.level = level + 1 // For glitch freedom 
    addDependent(handler)
  }
  def -=(handler: Dependent) = removeDependent(handler)
  
  def change[U >: T]: Event[(U, U)] = new ChangedEventNode[(U, U)](this)
 
  /* Testing */
  val timestamps = ListBuffer[Stamp]()
}



/**
 * Create a Signal
 */
object StaticSignal {
  
  def apply[T](reactivesDependsOn: List[DepHolder])(expr: => T) =
    new StaticSignal(reactivesDependsOn)(expr)
  
  type DH = DepHolder
  def apply[T]()(expr: =>T): Signal[T] = apply(List())(expr)
  def apply[T](r1: DH)(expr: =>T): Signal[T] = apply(List(r1))(expr)
  def apply[T](r1: DH,r2: DH)(expr: =>T): Signal[T] = apply(List(r1,r2))(expr)
  def apply[T](r1: DH,r2: DH,r3: DH)(expr: =>T): Signal[T] = apply(List(r1,r2,r3))(expr)
  def apply[T](r1: DH,r2: DH,r3: DH,r4: DH)(expr: =>T): Signal[T] = apply(List(r1,r2,r3,r4))(expr)
  def apply[T](r1: DH,r2: DH,r3: DH,r4: DH,r5: DH)(expr: =>T): Signal[T] = apply(List(r1,r2,r3,r4,r5))(expr)
}


/**
 * A callback called when a signal changes
 */
class Handler[T] (exp: =>T) extends Dependent {
    override def dependsOnchanged(change: Any,dep: DepHolder) = exp
    def triggerReevaluation = exp
}
object Handler{
	def apply[T] (exp: =>T) = new Handler(exp)
	//def apply[T] (fun: T=>Unit) = new Handler(fun)
}


/**
 * Then engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph. 
 */
object ReactiveEngine {
  
  var evalQueue = new PriorityQueue[Dependent]

  /* The queue behaves as an ordered set. Duplicates are discarded */
  def addToEvalQueue(dep: Dependent): Unit = {
    if (evalQueue.exists(_ eq dep)) return
    evalQueue += dep
  }
  def removeFromEvalQueue(dep: Dependent) = evalQueue = evalQueue.filter(_ eq dep)
  
  /* Evaluates all the elements in the queue */
  def startEvaluation = {
    while (!evalQueue.isEmpty) {
      val head = evalQueue.dequeue
      head.triggerReevaluation
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












