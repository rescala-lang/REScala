package rescala

import scala.collection.mutable.ListBuffer
import rescala.events.Event
import rescala.events.ChangedEventNode
import react.events.EventNode

//trait FixedDepHolder extends Reactive {
//  val fixedDependents = new ListBuffer[Dependent]
//  def addFixedDependent(dep: Dependent) = fixedDependents += dep
//  def removeFixedDependent(dep: Dependent) = fixedDependents -= dep
// def notifyDependents(change: Any): Unit = dependents.map(_.dependsOnchanged(change,this))
//}

/* A node that has nodes that depend on it */
class VarSynt[T](initval: T) extends Var[T] {
  private[this] var value: T = initval

  def get = value

  def set(newval: T): Unit = {

    val old = value
    // support mutable values by using hashValue rather than ==
    //val hashBefore = old.hashCode
    if (old != newval) {
      ReactiveEngine.log.nodeEvaluationStarted(this)

      value = newval
      TS.nextRound // Testing
      timestamps += TS.newTs // testing

      ReactiveEngine.log.nodeEvaluationEnded(this)

      notifyDependents(value)
      ReactiveEngine.startEvaluation

    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      //DEBUG: System.err.println("DEBUG OUTPUT: no update: " + newval + " == " + value)
      timestamps += TS.newTs // testing
    }
  }

  def update(v: T) = set(v)

  def apply() = get

  def reEvaluate(): T = value

  def map[B](f: T => B): Var[B] = VarSynt(f(get))
}

object VarSynt {
  def apply[T](initval: T) = new VarSynt(initval)
}

/* A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class SignalSynt[+T](reactivesDependsOnUpperBound: List[DepHolder])(expr: SignalSynt[T] => T)
  extends DependentSignal[T] {

  def this(expr: SignalSynt[T] => T) = this(List())(expr)

  reactivesDependsOnUpperBound.map(r => { // For glitch freedom
    if (r.level >= level) level = r.level + 1
  })

  /* Initial evaluation */
  val reactivesDependsOnCurrent = ListBuffer[DepHolder]()
  private[this] var currentValue = reEvaluate()

  def get = currentValue

  def triggerReevaluation() = reEvaluate

  def reEvaluate(): T = {

    /* Collect dependencies during the evaluation */
    reactivesDependsOnCurrent.map(_.removeDependent(this)) // remove me from the dependencies of the vars I depend on !
    reactivesDependsOnCurrent.clear
    timestamps += TS.newTs // Testing

    // support mutable values by using hashValue rather than ==
    //val hashBefore = currentValue.hashCode
    ReactiveEngine.log.nodeEvaluationStarted(this)
    val tmp = expr(this) // Evaluation)
    ReactiveEngine.log.nodeEvaluationEnded(this)
    //val hashAfter = tmp.hashCode

    setDependOn(reactivesDependsOnCurrent)
    reactivesDependsOnCurrent.map(_.addDependent(this))

    /* Notify dependents only of the value changed */
    if (currentValue != tmp) {
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
    ReactiveEngine.addToEvalQueue(this)
  }

  def apply() = get

  def map[B](f: T => B): Signal[B] =
    SignalSynt(List(this)) { s: SignalSynt[B] => f(this(s)) }
}

/**
 * A syntactic signal
 */
object SignalSynt {

  def apply[T](reactivesDependsOn: List[DepHolder])(expr: SignalSynt[T] => T) =
    new SignalSynt(reactivesDependsOn)(expr)

  type DH = DepHolder

  def apply[T](expr: SignalSynt[T] => T): SignalSynt[T] = apply(List())(expr)
  def apply[T](r1: DH)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(List(r1))(expr)
  def apply[T](r1: DH, r2: DH)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(List(r1, r2))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(List(r1, r2, r3))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH, r4: DH)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(List(r1, r2, r3, r4))(expr)
  def apply[T](r1: DH, r2: DH, r3: DH, r4: DH, r5: DH)(expr: SignalSynt[T] => T): SignalSynt[T] = apply(List(r1, r2, r3, r4, r5))(expr)
}





/** A wrappend event inside a signal, that gets "flattened" to a plain event node */
class WrappedEvent[T](wrapper: Signal[Event[T]]) extends EventNode[T] with Dependent {
  
  var inQueue = false
  var currentEvent = wrapper.getValue
  var currentValue: T = _
  
  // statically depend on wrapper
  wrapper.addDependent(this)
  addDependOn(wrapper)
  // dynamically depend on inner event
  currentEvent.addDependent(this)
  addDependOn(currentEvent)
  
  protected def updateProxyEvent(newEvent: Event[T]){
    // untie from from current event stream
    currentEvent.removeDependent(this)
    removeDependOn(currentEvent)
    // tie to new event stream
    newEvent.addDependent(this)
    addDependOn(newEvent)
  }
  
  def triggerReevaluation {
    timestamps += TS.newTs
    notifyDependents(currentValue)
    inQueue = false
  }
  
  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    
    if(dep eq wrapper) { // wrapper changed the proxy event
	    val newEvent = change.asInstanceOf[Event[T]]
	    if(newEvent ne currentEvent)
	      updateProxyEvent(newEvent)
    }
    else if(dep eq currentEvent) { // proxied event changed
      // store the value to propagate
      currentValue = change.asInstanceOf[T]
      
      // and queue this node
      if (!inQueue) {
    	  inQueue = true
    	  ReactiveEngine.addToEvalQueue(this)
      }
    }    
    else throw new IllegalStateException("Illegal DepHolder " + dep)

  }
  
  val timestamps = ListBuffer[Stamp]()
}

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)
  extends Dependent with DepHolder with Signal[T] {

  // The value of this signal
  private[this] var currentValue: T = init

  // The cached value of the last occurence of e
  private[this] var lastEvent: E = _

  private[this] var inQueue = false

  // Testing
  val timestamps = ListBuffer[Stamp]()

  def getValue = currentValue
  def getVal = currentValue

  def apply(): T = currentValue
  def apply(s: SignalSynt[_]) = {
    if (level >= s.level) s.level = level + 1
    s.reactivesDependsOnCurrent += this
    getVal
  }

  // The only dependant is e
  addDependOn(e)
  e.addDependent(this)
  this.level = e.level + 1

  def triggerReevaluation() = reEvaluate

  def reEvaluate(): T = {
    inQueue = false

    val hashBefore = if (currentValue == null) 0 else currentValue.hashCode
    ReactiveEngine.log.nodeEvaluationStarted(this)
    val tmp = f(currentValue, lastEvent)
    ReactiveEngine.log.nodeEvaluationEnded(this)
    val hashAfter = if (tmp == null) 0 else tmp.hashCode
    // support mutable values by using hashValue rather than ==
    if (hashAfter != hashBefore) {
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
    if (dep eq e) {
      lastEvent = change.asInstanceOf[E]
    } else {
      // this would be an implementation error
      throw new RuntimeException("Folded Signals can only depend on a single event node")
    }

    if (!inQueue) {
      inQueue = true
      ReactiveEngine.addToEvalQueue(this)
    }
  }

  def change[U >: T]: Event[(U, U)] = new ChangedEventNode[(U, U)](this)
}


/** Special reactive node for the "switch" interface function */
class SwitchedSignal[+T, +E](e: Event[E], init: Signal[T], factory: IFunctions.Factory[E, T])
  extends Dependent with DepHolder with Signal[T] {

  // The "inner" signal
  private[this] var currentSignal: Signal[T] = init
  // the current factory being called on the next occurence of e
  private[this] var currentFactory: IFunctions.Factory[E, T] = factory

  private[this] var inQueue = false

  // Testing
  val timestamps = ListBuffer[Stamp]()

  def getValue = currentSignal.getValue
  def getVal = currentSignal.getVal

  def apply(): T = currentSignal.apply()
  def apply(s: SignalSynt[_]) = {
    if (level >= s.level) s.level = level + 1
    s.reactivesDependsOnCurrent += this
    getVal
  }

  private def removeInner(s: Signal[_]) {
    dependOn -= s
    s.removeDependent(this)
  }

  private def addInner(s: Signal[_]) {
    dependOn += s
    s.addDependent(this)
    this.level = math.max(e.level, s.level) + 1
  }

  // Switched signal depends on event and the current signal
  dependOn += e
  e.addDependent(this)
  addInner(currentSignal)

  def triggerReevaluation() = reEvaluate

  def reEvaluate(): T = {
    inQueue = false

    ReactiveEngine.log.nodeEvaluationStarted(this)
    val inner = currentSignal.reEvaluate
    ReactiveEngine.log.nodeEvaluationEnded(this)
    inner
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (dep eq e) {
      val event = change.asInstanceOf[E]
      val (newSignal, newFactory) = currentFactory.apply(event)
      if (newSignal ne currentSignal) {
        removeInner(currentSignal)
        currentSignal = newSignal
        currentFactory = newFactory
        addInner(currentSignal)
      }
      // hack?
      val value = reEvaluate
      notifyDependents(value)
    } else {
    }

    if (!inQueue) {
      inQueue = true
      ReactiveEngine.addToEvalQueue(this)
    }
  }

  def change[U >: T]: Event[(U, U)] = new ChangedEventNode[(U, U)](this)
}
