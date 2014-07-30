package rescala.events

import rescala.events.EventNodeExcept.State

import scala.collection.LinearSeq
import rescala._



trait Event[+T] extends DepHolder {

  def +=(react: T => Unit): Unit

  def -=(react: T => Unit): Unit

  /**
   * Events disjunction.
   */
  def ||[S >: T, U <: S](other: Event[U]) = new EventNodeOr[S](this, other)

  /**
   * Event filtered with a predicate
   */
  def &&[U >: T](pred: U => Boolean) = new EventNodeFilter[U](this, pred)
  def filter[U >: T](pred: U => Boolean) = &&[U](pred)

  /**
   * Event filtered with a boolean variable
   */
  def &&[U >: T](pred: =>Boolean) = new EventNodeFilter[U](this, _ => pred)
  def filter[U >: T](pred: =>Boolean) = &&[U](pred)

  /**
   * Event is triggered except if the other one is triggered
   */
  def \[U >: T](other: Event[U]) = new EventNodeExcept[U](this, other)

  /**
   * Events conjunction
   */
  def and[U, V, S >: T](other: Event[U], merge: (S, U) => V) = new EventNodeAnd[S, U, V](this, other, merge)

  /**
  * Event conjunction with a merge method creating a tuple of both event parameters
  */
  def &&[U, S >: T](other: Event[U]) = new EventNodeAnd[S, U, (S, U)](this, other, (p1: S, p2: U) => (p1, p2))

  /**
   * Transform the event parameter
   */
  def map[U, S >: T](mapping: S => U) = new EventNodeMap[S, U](this, mapping)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  def dropParam[S >: T] = new EventNodeMap[S, Unit](this, (_: Any) => ())




  ///** The latest parameter value of this event occurrence */
  //import annotation.unchecked.uncheckedVariance
  //lazy val latest : Signal[Option[T @uncheckedVariance]] = Signal.latestOption(this)

  // def hold: Signal[T] =

  def fold[A](init: A)(fold: (A, T) => A): Signal[A] = IFunctions.fold(this, init)(fold)
  def iterate[A](init: A)(f: A => A): Signal[A] = IFunctions.iterate(this, init)(f)
  def count: Signal[Int] = IFunctions.count(this)

  def set[B >: T,A](init: B)(f: (B=>A)): Signal[A] = IFunctions.set(this,init)(f)

  def latest[S >: T](init: S): Signal[S] = IFunctions.latest(this, init)
  def hold[S >: T]:  Signal[Option[T]] = IFunctions.latestOption[T](this)
  def latestOption[S >: T]:  Signal[Option[T]] = IFunctions.latestOption[T](this)

  def reset[S >: T, A](init : S)(f : (S) => Signal[A]) : Signal[A] = IFunctions.reset(this, init)(f)

  def last[S >: T](n: Int): Signal[LinearSeq[S]] = IFunctions.last[S](this, n)
  def list[S >: T](): Signal[List[S]] = IFunctions.list[S](this)

  def toggle[A](a: Signal[A], b: Signal[A]): Signal[A] = IFunctions.toggle(this, a, b)
  def snapshot[A](s: Signal[A]): Signal[A] = IFunctions.snapshot(this, s)

  def switchOnce[A](oldS: Signal[A], newS: Signal[A]): Signal[A] = IFunctions.switchOnce(this, oldS, newS)

  def delay[S >: T](init: S, n: Int): Signal[S] = IFunctions.delay(this, init, n)
  // TODO: make another delay that returns an event

}



/**
 * Wrapper for an anonymous function
 */
case class EventHandler[T](fun: T => Unit) extends Dependent {
  override def dependsOnchanged(change: Any, dep: DepHolder): Unit = fun(change.asInstanceOf[T])
  def triggerReevaluation() = {}
}

/**
 *  Base trait for events.
 */
trait EventNode[T] extends Event[T] {
  def +=(react: T => Unit): Unit = addDependent(EventHandler(react))
  def -=(react: T => Unit): Unit = removeDependent(EventHandler(react))
}


/**
 * An implementation of an imperative event
 */
class ImperativeEvent[T] extends EventNode[T] {

  /** Trigger the event */
  def apply(v: T): Unit = {
    TS.nextRound()
    logTestingTimestamp()
    notifyDependents(v)
    ReactiveEngine.startEvaluation()
  }

  override def toString = getClass.getName
}


/**
 * depends on a single reactive and basically “folds“ the incoming events with the store function into some state
 * the trigger function then allows to map and filter that state to produce this events change
 */
abstract class UnaryStoreTriggerNode[StoreType, TriggeredType, DependencyType <: DepHolder]
  (dependency: DependencyType,
   private var storedValue: Option[StoreType] = None)
  extends EventNode[TriggeredType] with DepHolder with Dependent {

  addDependOn(dependency)

  /** this method is called to produce a new change. if it returns None no change is propagated, alse the returned value is propagated. */
  def trigger(stored: Option[StoreType]): Option[TriggeredType]
  /** takes the current state and the incoming change and produces a new state */
  def store(stored: Option[StoreType], change: Any): Option[StoreType] = Some(change.asInstanceOf[StoreType])

  def triggerReevaluation(): Unit = {
    logTestingTimestamp() // Testing
    trigger(storedValue).foreach(notifyDependents)
  }

  override def dependsOnchanged(change: Any,dep: DepHolder) = {
    storedValue = store(storedValue, change)
    ReactiveEngine.addToEvalQueue(this)
  }
  
}

/**
 * takes two events and combines their values with the storeA and storeB methods
 * can filter and map the combined result with the trigger method
 * the current state is reset after every turn
 */
abstract class BinaryStoreTriggerNode[StoreType, TriggeredType, T1, T2]
  (dependencyA: Event[T1],
   dependencyB: Event[T2],
   resetState: => Option[StoreType] = None)
  extends EventNode[TriggeredType] with DepHolder with Dependent {

  private var storedValue: Option[StoreType] = resetState

  addDependOn(dependencyA)
  addDependOn(dependencyB)

  /** this method is called to produce a new change. if it returns None no change is propagated, alse the returned value is propagated. */
  def trigger(stored: Option[StoreType]): Option[TriggeredType]
  /** updates the stored state on changes of the first Event */
  def storeA(stored: Option[StoreType], change: T1): Option[StoreType]
  /** updates the stored state on changes of the second Event */
  def storeB(stored: Option[StoreType], change: T2): Option[StoreType]

  def triggerReevaluation(): Unit = {
    logTestingTimestamp() // Testing
    trigger(storedValue).foreach(notifyDependents)
    storedValue = resetState
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (dep eq dependencyA) {
      storedValue = storeA(storedValue, change.asInstanceOf[T1])
    }
    else if (dep eq dependencyB) {
      storedValue = storeB(storedValue, change.asInstanceOf[T2])
    }
    else {
      throw new IllegalStateException(s"illegal dependency $dep")
    }
    ReactiveEngine.addToEvalQueue(this)
  }

}


/**
 * Used to model the change event of a signal. Keeps the last value
 */
class ChangedEventNode[T](dependency: DepHolder)
  extends UnaryStoreTriggerNode[(Any, Any), (T , T), DepHolder](dependency, storedValue = Some((null, null))) {
  override def store(stored: Option[(Any, Any)], change: Any): Option[(Any, Any)] = stored.map{ case (_, old) => (old, change): (Any, Any) }
  override def trigger(stored: Option[(Any, Any)]): Option[(T, T)] = stored.map(_.asInstanceOf[(T, T)])
  override def toString = "(" + " InnerNode" + dependency + ")"
}


// TODO: never used
/**
 * An event automatically triggered by the framework.
 */
class InnerEventNode[T](dependency: DepHolder) extends UnaryStoreTriggerNode[T, T, DepHolder](dependency) {
  override def trigger(stored: Option[T]): Option[T] = stored
  override def toString = "(" + " InnerNode" + dependency + ")"
}



/**
 * Implementation of event disjunction
 */
class EventNodeOr[T](ev1: Event[_ <: T], ev2: Event[_ <: T])
    extends BinaryStoreTriggerNode[T, T, T, T](ev1, ev2) {

  override def trigger(stored: Option[T]): Option[T] = stored
  override def storeB(stored: Option[T], change: T): Option[T] = Some(change)
  override def storeA(stored: Option[T], change: T): Option[T] = Some(change)

  override def toString = "(" + ev1 + " || " + ev2 + ")"
}


/**
 * Implementation of event conjunction
 */
class EventNodeAnd[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T)
  extends BinaryStoreTriggerNode[(Option[T1], Option[T2]), T, T1, T2](ev1, ev2, Some((None, None))) {

  override def trigger(stored: Option[(Option[T1], Option[T2])]): Option[T] =
    for { (leftOption, rightOption) <- stored; left <- leftOption; right <- rightOption }
    yield { merge(left, right) }

  override def storeA(stored: Option[(Option[T1], Option[T2])], change: T1): Option[(Option[T1], Option[T2])] = stored.map { _.copy(_1 = Some(change)) }
  override def storeB(stored: Option[(Option[T1], Option[T2])], change: T2): Option[(Option[T1], Option[T2])] = stored.map { _.copy(_2 = Some(change)) }

  override def toString = "(" + ev1 + " and " + ev2 + ")"
}





/**
 * Implements filtering event by a predicate
 */
class EventNodeFilter[T](ev: Event[T], f: T => Boolean) extends UnaryStoreTriggerNode[T, T, Event[T]](ev) {
  override def trigger(stored: Option[T]): Option[T] = stored.filter(f)
  override def toString = "(" + ev + " && <predicate>)"
}


/**
 * Implements transformation of event parameter
 */
class EventNodeMap[T, U](ev: Event[T], f: T => U) extends UnaryStoreTriggerNode[T, U, Event[T]](ev) {
  override def trigger(stored: Option[T]): Option[U] = stored.map(f)
  override def toString = "(" + ev + " && <predicate>)"
}


/**
 * Implementation of event except
 */
class EventNodeExcept[T](accepted: Event[T], except: Event[T])
  extends BinaryStoreTriggerNode[EventNodeExcept.State[T], T, T, T](accepted, except, Some(
    EventNodeExcept.State(
      currentValue = None,
      gotExcept = false))) {

  override def trigger(stored: Option[State[T]]): Option[T] = stored.flatMap { state =>
    if (!state.gotExcept) state.currentValue else None
  }

  override def storeA(stored: Option[State[T]], change: T): Option[State[T]] = stored.map { _.copy(currentValue = Some(change)) }
  override def storeB(stored: Option[State[T]], change: T): Option[State[T]] = stored.map { _.copy(gotExcept = true) }

  override def toString = "(" + accepted + " \\ " + except + ")"
}

object EventNodeExcept {
  case class State[T](currentValue: Option[T], gotExcept: Boolean)
}


object emptyevent extends Event[Nothing] {
  def +=(react: Nothing => Unit) { /* do nothing */ }
  def -=(react: Nothing => Unit) { /* do nothing */ }
}





/**
 * Implementation of an observable method
 */
class Observable[T, U](body: T => U) extends (T => U) {
  // before and after, modeled as primitive events
  lazy val before = new ImperativeEvent[T]
  lazy val after = new ImperativeEvent[(T,U)]

  /**
  * Instrumented method implementation:
  * trigger events before and after the actual method execution
  */
  def apply(t: T): U = {
    before(t)
    val res = body(t)
    after((t, res))
    res
  }
}

object Observable {
  def apply[T,U](f: T => U) = new Observable(f)
}
