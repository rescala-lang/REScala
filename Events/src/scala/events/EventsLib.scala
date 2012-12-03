package scala.events

import scala.collection.mutable.{ListBuffer,Stack}
import scala.util.DynamicVariable
import scala.events.behaviour.Signal
import scala.events.behaviour.Var

trait Event[+T] {
  /*
   * Sink function type: takes event id, event parameter
   * fills the given list with reactions to be executed
   * the reactions represt
   */
  type Sink = (Int, T, ListBuffer[(() => Unit, Trace)]) => Unit
  type Trace = List[Event[_]]

  /*
   * Register a sink function to the event (used for propagation of events)
   */
  protected[events] def +=(sink: Sink)

  /*
   * Unregister a sink function
   */
  protected[events] def -=(sink: Sink)

  /*
   * Register a reaction to the event
   */
  def +=(react: T => Unit)

  /*
   * Unregister a reaction
   */
  def -=(react: T => Unit)

  /**
   * Events disjunction.
   */
  def ||[S >: T, U <: S](other: Event[U]) = new EventNodeOr[S](this, other)

  /**
   * Creates an events sequence
   */
  def then[U, V, S >: T](other: => Event[U], merge: (S, U) => V) = new EventNodeSequence[S, U, V](this, other, merge)

  /**
   * Creates an events sequence with a merge method creating a tuple of both event parameters
   */
  def then[U, S >: T](other: => Event[U]) = new EventNodeSequence[S, U, (S, U)](this, other, (p1: S, p2: U) => (p1, p2))

  /**
   * Event filtered with a predicate
   */
  def &&[U >: T, V >: T <: U](pred: U => Boolean) = new EventNodeFilter[V](this, pred)

  /**
   * Event filtered with a boolean variable
   */
  def &&[U >: T](pred: () => Boolean) = new EventNodeFilter[U](this, _ => pred())

  def &&[U >: T](itp: IntervalEventFilter) = new EventNodeFilterInterval[U](this, itp)
  
  /**
   * Event is triggered except if the other one is triggered
   */
  def \[U >: T,S](other: Event[S]) = new EventNodeExcept[U,S](this, other)

  /**
   * Events conjunction
   */
  def and[U, V, S >: T](other: Event[U], merge: (S, U) => V) = new EventNodeAnd[S, U, V](this, other, merge)

  /*
  * Event conjunction with a merge method creating a tuple of both event parameters
  */
  def and[U, S >: T](other: Event[U]) = new EventNodeAnd[S, U, (S, U)](this, other, (p1: S, p2: U) => (p1, p2))

  /**
   * Transform the event parameter
   */
  def map[U, S >: T, V >: T <: S](mapping: S => U) = new EventNodeMap[V, U](this, mapping)

  /**
   * Transform the event parameter independently of the parameter value
   */
  //def map[U, S >: T](mapping: () => U) = new EventNodeMap[S, U](this, _ => mapping())

  /**
   * Event is triggered if the first event was already triggered but not the second one yet
   */
   //def between[U, V, S >: T](e1: Event[U], e2: Event[V]) = new BetweenEventNode[S,U,V](this, e1, e2)
   //def between[U, V, S >: T](ep: (Event[U], Event[V])) = new BetweenEventNode[S,U,V](this, ep._1, ep._2)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  def dropParam[S >: T] = new EventNodeMap[S, Unit](this, _ => ())
 
  // NOTE:
  // The Signal functions are all defined in the Signal class, so that the Event class does not not be modified.
  def fold[A](init : A)(fold : (A,T) => A): Signal[A] = Signal.fold(this, init)(fold)
  def latest[S >: T](init : S) : Signal[S] = Signal.latest(this, init)
  def iterate[A](init : A)(f : A => A) : Signal[A] = Signal.iterate(this, init)(f)
  def reset[S >: T, A](init : S)(f : (S) => Signal[A]) : Signal[A] = Signal.reset(this, init)(f)
  def last[S >: T](n : Int) : Signal[List[S]] = Signal.last(this, n)
  def list[S >: T]() : Signal[List[S]] = Signal.list(this)
  def toggle[A](a: Signal[A], b: Signal[A]): Signal[A] = Signal.toggle(this, a, b)
  def snapshot[A](s : Signal[A]): Signal[A] =  s snapshot this
    
  /** The latest parameter value of this event occurrence */
  import annotation.unchecked.uncheckedVariance
  lazy val latest : Signal[Option[T @uncheckedVariance]] = Signal.latestOption(this)
  
  
// This code is proof-of-concept
//  /** Generates a signal from an event occurrence */
//  trait Factory[E, A] {
//    def apply(eventValue : E) : (Signal[A], Factory[E,A])
//  }
//  
//  /** Generates a signal from an initial signal and a factory for subsequent ones */
//  def switch[S >: T, A](init : Signal[A])(factory : Factory[S,A]) : Signal[A] = {
//    import scala.events.behaviour.Var
//    val ref : Var[Signal[A]] = new Var(init)
//    val fac : Var[Factory[S,A]] = new Var(factory)
//    
//    val handleSignal = {(eventValue : T) =>
//      val currentFactory = fac()
//      val (newSignal, newFactory) = currentFactory(eventValue)
//      fac() = newFactory
//      ref() = newSignal
//    }
//    
//    this += handleSignal
//    return Signal { ref()()}
//  }
}

/*
 *  Base class for events.
 *  Maintains sinks and reactions and
 */
abstract class EventNode[T] extends Event[T] {
  protected val sinks = new ListBuffer[Sink]
  protected[events] val _reactions = new ListBuffer[T => Unit]

  /*
   * Register a reaction to the event
   */
  protected[events] def +=(sink: Sink) {
    sinks += sink
    // deploy the event if the first reaction/sink is registered
    if (sinks.size == 1 && _reactions.size == 0)
      deploy
  }


  protected[events] def -=(sink: Sink) {
    sinks -= sink
    // undeploy the event if the last reaction/sink is unregistered
    // has cascade effect
    // avoids redundant event propagation
    if (sinks.size == 0 && _reactions.size == 0)
      undeploy
  }

  /*
   * Register a reaction to the event
   */
  def +=(react: T => Unit) {
    _reactions += react
    // deploy the event if the first reaction/sink is registered
    if (_reactions.size == 1 && sinks.size == 0)
      deploy
  }

  /*
   * Unregister a reaction
   */
  def -=(react: T => Unit) {
    _reactions -= react
    // undeploy the event if the last reaction/sink is unregistered
    if (sinks.size == 0 && _reactions.size == 0)
      undeploy
  }

  /**
   * Deploy the event (i.e. registers to the referenced events)
   */
  protected def deploy: Unit

  /**
   * Undeploy the event (i.e. unregisters from the referenced events)
   */
  protected def undeploy: Unit

  /** Collects the reactions registered with this event and associates the current event trace.
   *  It then propagates to sinks.
   */
  protected[events] def reactions(id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) {
    eventTrace.withValue(this :: eventTrace.value) {
      // collect the reactions registered with this event
      _reactions.foreach(react => reacts += ((() => react(v)) -> eventTrace.value))
      // propagate to sinks adding this event to the event trace
      sinks.foreach(sink => sink(id, v, reacts))
    }
  }
}

protected[events] object eventTrace extends DynamicVariable[List[Event[_]]](Nil)

object EventIds {
  // last used event id
  var lastId: Int = 0

  /*
  * generate new event id
  */
  def newId(): Int = {
    lastId += 1
    lastId
  }
}

/*
 * An implementation of an explicit event
 */
class ImperativeEvent[T] extends EventNode[T] {

  private var deployed = false
  
  /*
  * Trigger the event
  */
  def apply(v: T) = {
    beforeTrigger(v)
    // does something only if the event is deployed, i.e. if some reactions or sinks
    // are registered
    if(deployed) {
      // collect matching reactions
      val reacts: ListBuffer[(() => Unit, Trace)] = new ListBuffer

      reactions(EventIds.newId(), v, reacts)
      // once reactions are collected, we are after the triggering
      afterTrigger(v)
      // execute the collected reactions
      reacts.foreach(_ match {
        case (react, trace) => {
          eventTrace.withValue(trace) {
            //try {
              react()
            /*} catch {
              case e => 
                println("Event trace:")
                println(eventTrace.value.mkString("", "\n", "\n"))
                throw e
            }*/
          }
        }}
      )
    } else {
      afterTrigger(v)
    }
  }

  protected[events] def beforeTrigger(v: T) {}
  protected[events] def afterTrigger(v: T) {}

  protected override def deploy { deployed = true }

  protected override def undeploy { deployed = false }

  override def toString = getClass.getName
  
  
    
  // Gerold: Added the possibility to register 'child events' with imperative events
  lazy val sinkMap : collection.mutable.Map[EventNode[T], Sink] = new collection.mutable.HashMap()
  def sinkForEvent(target : EventNode[T]) : Sink = {
    lazy val react = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
      target.reactions(id, v, reacts)
    }
    sinkMap.getOrElseUpdate(target, react)
  }
  
  def +=:(target : EventNode[T]){
    this += sinkForEvent(target)
  }
  
  def -=:(target : EventNode[T]){
    this -= sinkForEvent(target)
  }
  

}

/*
 * Implementation of event conjunction
 */
class EventNodeAnd[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T) extends EventNode[T] {

  // The id of the last received event
  var id: Int = 0

  // Parameter value of event1 or event2 (depending on which is received first)
  var v1: T1 = _
  var v2: T2 = _
  var currentTrace: Trace = Nil

  /*
  * Reaction to event1
  */
  lazy val onEvt1 = (id: Int, v1: T1, reacts: ListBuffer[(() => Unit, Trace)]) => {
    if (this.id == id) {
      // event2 is already received; collect the reactions
      eventTrace.withValue(currentTrace ::: eventTrace.value) {
        reactions(id, merge(v1, this.v2), reacts)
      }
    }
    else {
      // event2 is not received yet; save the data of the event1
      this.id = id
      this.v1 = v1
      this.currentTrace = eventTrace.value
    }
  }

  /*
   * Reaction to event2
   */
  lazy val onEvt2 = (id: Int, v2: T2, reacts: ListBuffer[(() => Unit, Trace)]) => {
    if (this.id == id) {
      // event1 is already received; collect the reactions
      eventTrace.withValue(currentTrace ::: eventTrace.value) {
        reactions(id, merge(this.v1, v2), reacts)
      }
    }
    else {
      // event1 is not received yet; save the data of the event2
      this.id = id
      this.v2 = v2
      this.currentTrace = eventTrace.value
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt1
    ev2 += onEvt2
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt1
    ev2 -= onEvt2

  }

  override def toString = "(" + ev1 + " and " + ev2 + ")"

}

/*
 * Implementation of event disjunction
 */
class EventNodeOr[T](ev1: Event[_ <: T], ev2: Event[_ <: T]) extends EventNode[T] {

  // The id of the last received event
  var id: Int = 0

  /*
   * Reaction to both events
   */
  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // if the event was already received, avoid processing it twice
    if (this.id != id) {
      this.id = id
      // the event received for the first time; collect the reactions
      reactions(id, v, reacts)
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt
    ev2 += onEvt
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt
    ev2 -= onEvt
  }

  override def toString = "(" + ev1 + " || " + ev2 + ")"
}

/*
 * Implements transformation of event parameter
 */
class EventNodeMap[T, U](ev: Event[T], f: T => U) extends EventNode[U] {

  /*
   * Reaction to the referenced event
   */
  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // transform v to f(v)
    reactions(id, f(v), reacts)
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev += onEvt
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev -= onEvt
  }

  override def toString = getClass.getName

}

/*
 * Implements filtering event by a predicate
 */
class EventNodeFilter[T](ev: Event[T], f: T => Boolean) extends EventNode[T] {

  /*
   * Reaction to the referenced event
   */
  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // filter the event by f(v)
    if (f(v)) {
      reactions(id, v, reacts)
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev += onEvt
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev -= onEvt
  }

  override def toString = "(" + ev + " && <predicate>)"

}

/*
 * Implements reference to an event of an object (referenced by a variable)
 */
class EventNodeRef[T, U](target: Variable[T], evf: T => Event[U]) extends EventNode[U] {
  
  /*
  * Currently referenced event
  */
  private var ev: Event[U] = if(target.value != null) evf(target.value) else emptyevent

  import EventsLibConversions._
  
  /*
   * Reaction to a change of the target
   */
  lazy val onTargetChanged = toTupledFun2((oldTarget: T, newTarget: T) => {
    // unregister from the current event
    ev -= onEvt
    // retrieve and save the new event
    if(newTarget != null)
      ev = evf(newTarget)
    else 
      ev = emptyevent
    // register to the new event
    ev += onEvt
  })

  /*
   * Reaction to the currently referenced event
   */
  lazy val onEvt = (id: Int, v: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    reactions(id, v, reacts)
  }
  
  /*
  * Register to the referenced event and changes of the target
  */
  protected override def deploy {
    ev += onEvt
    target.changed += onTargetChanged
  }

  /*
  * Unregister from the referenced event and changes of the target
  */
  protected override def undeploy {
    ev -= onEvt
    target.changed -= onTargetChanged
  }

  override def toString = getClass.getName

}

/*
 * Implementation of quantification over a (varying) list of objects
 */
class EventNodeExists[T, U](list: VarList[T], evf: T => Event[U]) extends EventNode[U] {

  /*
   * Register to the event of a newly added list element
   */
  lazy val onElementAdded = (target: T) => {
    evf(target) += onEvt
  }

  /*
   * Unregister from the event of a removed list element
   */
  lazy val onElementRemoved = (target: T) => {
    evf(target) -= onEvt
  }

  /*
   * Reaction to the observed events
   */
  lazy val onEvt = (id: Int, v: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    reactions(id, v, reacts)
  }

  /*
  * Register to the events of all list elements and the list changes
  */
  protected override def deploy {
    list.foreach(target => evf(target) += onEvt)
    list.elementAdded += onElementAdded
    list.elementRemoved += onElementRemoved
  }

  /*
  * Unregister from the events of all list elements and the list changes
  */
  protected override def undeploy {
    list.foreach(target => evf(target) -= onEvt)
    list.elementAdded -= onElementAdded
    list.elementRemoved -= onElementRemoved
  }

  override def toString = getClass.getName

}

/*
 * Implementation of event sequence operator
 */
class EventNodeSequence[T, U, V](ev1: Event[T], ev2: => Event[U], merge: (T, U) => V) extends EventNode[V] {

  // the id of the last received event1
  // -1 if event1 was not received yet (or after last event2)
  var id: Int = -1
  // value of the last event1
  var v1: T = _

  /*
  * Reaction to event1
  */
  lazy val onEvt1 = (id: Int, v1: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // ignore consecutive occurrences of event1
    if (this.id == -1) {
      // save the data of event1
      this.id = id
      this.v1 = v1
    }
  }

  /*
   * Reaction to event2
   */
  lazy val onEvt2 = (id: Int, v2: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // react to event2 only if event1 was already received;
    // also ensure that event2 is different from event1 by comparing the ids
    if (this.id != -1 && this.id != id) {
      // collect the reactions
      reactions(id, merge(this.v1, v2), reacts)
      // reset the sequence
      this.id = -1
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt1
    ev2 += onEvt2
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt1
    ev2 -= onEvt2
  }

  override def toString = "(" + ev1 + " then " + ev2 + ")"

}

class EventNodeCond[T](event: =>Event[T]) extends EventNode[T] {

  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    reactions(id, v, reacts)
  }

  private def getEvent(ev: =>Event[T]): Event[T] =
    try {
      event
    } catch {
      case _: NullPointerException => emptyevent
    }

  override def deploy {
    getEvent(event) += onEvt
  }
  override def undeploy {
    getEvent(event) -= onEvt
  }
}

class EventNodeExcept[T,U](accpeted: Event[T], except: Event[U]) extends EventNode[T] {
  
  private val myReacts = new ListBuffer[(() => Unit, Trace)]
  
  private var id = -1
  
  lazy val onAccepted = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    myReacts.clear
    // if the id is already set, the except event was received
    if(this.id != id) {
      this.id = id
      reactions(id, v, reacts)
    }
  }
  
  lazy val onExcept = (id: Int, v: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // the except event is received, set the id to
    if(this.id != id) {
      this.id = id
    } else {
      // remove all my registered reactions
      reacts --= myReacts
      myReacts.clear
    }
  }
  
  override def reactions(id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) {
    eventTrace.withValue(this :: eventTrace.value) {
      // collect the reactions of this event
      _reactions.foreach(react => myReacts += (() => react(v), eventTrace.value))
      // collect the reactions of the sinks
      sinks.foreach(sink => sink(id, v, myReacts))
    }
    // add my reactions and my sinks reactions to the global reactions
    reacts ++= myReacts
  }
  
  override def deploy {
    accpeted += onAccepted
    except += onExcept
  }
  
  override def undeploy {
    accpeted -= onAccepted
    except -= onExcept
  }
}

class EventNodeFilterInterval[T](event: Event[T], itp: IntervalEventFilter) extends EventNode[T] {

  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    if(itp())
      reactions(id, v, reacts)
  }

  override def deploy {
    event += onEvt
    itp.deploy
  }

  override def undeploy {
    event -= onEvt
    itp.undeploy
  }
}

class CausedByFilter(e: Event[_]) extends Function0[Boolean] {
  def apply() = eventTrace.value.contains(e)
  def unary_! = () => !eventTrace.value.contains(e)
}

/*
 * Implementation of an observable variable
 */
class Variable[T](private var v: T) extends scala.events.reactive.Reactive[T] {
  def value: T = this.v

  def value_=(v: T) = {
    if(this.v != v) {
      val old = this.v
      this.v = v
      changed(old,v)
    }
  }

  def :=(v: T) = value_=(v)
  def apply(): T = value

  lazy val changed = new ImperativeEvent[(T,T)]
  
  /*
   * A convenience operator for referencing an event of the variable
   */
  def event[U](evf: T => Event[U]) = new EventNodeRef(this, evf)
}

object Variable {
  def apply[T](v: T) = new Variable(v)
  def unapply[T](v: Variable[T]): Option[T] = Some(v())
}

/*
 * Implementation of an observable list
 */
class VarList[T] private(private val buffer: ListBuffer[T]) extends Iterable[T] {

  def this() = this(new ListBuffer[T])


  // Use list buffer for implementation
  // private val buffer = new ListBuffer[T]

  // delegate the iterator implementation
  override def iterator = buffer.iterator

  /*
   * Add a new element to the list; trigger the corresponding event
   */
  def +=(v: T) = {buffer += v; elementAdded(v)}

  /*
  * Remove an element from the list; trigger the corresponding event
  */
  def -=(v: T) = {buffer -= v; elementRemoved(v)}

  def clear() = {
    buffer.foreach(v => elementRemoved(v))
    buffer.clear()
  }

  def reverse: VarList[T] = new VarList(buffer.reverse)

  //def find(fun: T => Boolean): Option[T] = buffer.find(fun)

  /*
   * A convenience operator creating an event based on the list
   */
  def any[U](evf: T => Event[U]) = new EventNodeExists(this, evf)
  
  /*
  * Events notifying over the changes in the list
  */
  lazy val elementAdded = new ImperativeEvent[T]
  lazy val elementRemoved = new ImperativeEvent[T]
}

object VarList {
  def apply[T](a: Array[T]) = {
    val result = new VarList[T]()
    a.foreach(v => result += v)
    result
  }
  def apply[T](values: Iterable[T]) = {
    val result = new VarList[T]()
    values.foreach(v => result += v)
    result
  }
}

/*
 * Implicit type conversions
 */
object EventsLibConversions {
  // implicitly to an observable variable
  implicit def toVariable[T](o: T) = Variable(o)
  // implicit or explicit lifting of methods for instrumentation
  implicit def lift[T, U](body: T => U) = Observable(body)
  // implicitly drop event parameter if not used
  implicit def dropParam[T](ev: Event[T]) = ev.dropParam

  // some implicit conversion for methods which allow to write 
  // instrumented methods in a more intuitive way.
  implicit def toUnitfun[T](f: () => T) = (_: Unit) => f()
  implicit def toTupledFun2[T1,T2,R](f: (T1,T2) => R) = f.tupled
  implicit def toTupledFun3[T1,T2,T3,R](f: (T1,T2,T3) => R) = f.tupled
  implicit def toTupledFun4[T1,T2,T3,T4,R](f: (T1,T2,T3,T4) => R) = f.tupled
  implicit def toTupledFun5[T1,T2,T3,T4,T5,R](f: (T1,T2,T3,T4,T5) => R) = f.tupled
  implicit def toTupledFun6[T1,T2,T3,T4,T5,T6,R](f: (T1,T2,T3,T4,T5,T6) => R) = f.tupled
  implicit def toTupledFun7[T1,T2,T3,T4,T5,T6,T7,R](f: (T1,T2,T3,T4,T5,T6,T7) => R) = f.tupled
  implicit def toTupledFun8[T1,T2,T3,T4,T5,T6,T7,T8,R](f: (T1,T2,T3,T4,T5,T6,T7,T8) => R) = f.tupled
  implicit def toTupledFun9[T1,T2,T3,T4,T5,T6,T7,T8,T9,R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => R) = f.tupled
  implicit def toTupledFun10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => R) = f.tupled
}

/*
 * Implementation of an observable method
 */
class Observable[T, U](body: T => U) extends (T => U) {
  // before and after, modeled as primitive events
  lazy val before = new ImperativeEvent[T]
  lazy val after = new ImperativeEvent[(T,U)]

  /*
  * Instrumented method implementation:
  * trigger events before and after the actual method execution
  */
  def apply(t: T): U = {
    before(t)
    val res = body(t)
    after(t, res)
    res
  }
}

object Observable {
  def apply[T,U](f: T => U) = new Observable(f)
}

