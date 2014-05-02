package react.events

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.LinearSeq
import scala.reflect.runtime.universe._
import react._



trait Event[+T] extends DepHolder {

  def +=(react: T => Unit)

  def -=(react: T => Unit)

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




  /** The latest parameter value of this event occurrence */
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

  def switchOnce[T](oldS: Signal[T], newS: Signal[T]): Signal[T] = IFunctions.switchOnce(this, oldS, newS)

  def delay[S >: T](init: S, n: Int): Signal[S] = IFunctions.delay(this, init, n)
  // TODO: make another delay that returns an event

}



/**
 * Wrapper for an anonymous function
 */
class EventHandler[T] (fun: T=>Unit) extends Dependent {
    val f = fun
    var storedVal: T = _
    override def dependsOnchanged(change: Any, dep: DepHolder) {
      storedVal = change.asInstanceOf[T]  // ??
      ReactiveEngine.addToEvalQueue(this)
    }
    def triggerReevaluation = fun(storedVal)
    override def equals(other: Any) = other match {
      case other: EventHandler[T] => fun.equals(other.f)
      case _ => false
    }
}
object EventHandler {
	def apply[T] (fun: T=>Unit) = new EventHandler(fun)
}


/*
 *  Base class for events.
 */
abstract class EventNode[T] extends Event[T] with DepHolder {

  // memorize handler wrappers, so we can remove them
  lazy val handlers : collection.mutable.Map[(T => Unit), EventHandler[T]] =
    new collection.mutable.HashMap()

  def getHandler(react: T => Unit) : EventHandler[T] =
    handlers.getOrElseUpdate(react, EventHandler(react))

  def +=(react: T => Unit) = this addDependent getHandler(react)
  def -=(react: T => Unit) = this removeDependent getHandler(react)
}


/*
 * An implementation of an imperative event
 */
class ImperativeEvent[T] extends EventNode[T] {

  /* Trigger the event */
  def apply(v: T): Unit = {
    TS.nextRound
    timestamps += TS.newTs
    notifyDependents(v)
    ReactiveEngine.startEvaluation
  }

  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = getClass.getName
}



/*
 * Used to model the change event of a signal. Keeps the last value
 */
class ChangedEventNode[T](d: DepHolder) extends EventNode[T] with Dependent {

  level = d.level + 1 // Static, for glitch freedom
  d.addDependent(this) // To be notified in the future
  dependOn += d

  var storedVal: (Any,Any) = (null, null)

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    notifyDependents(storedVal)
  }

  override def dependsOnchanged(change: Any,dep: DepHolder) = {
    storedVal = (storedVal._2,change)
    ReactiveEngine.addToEvalQueue(this)
  }

  /* Testing */
  val timestamps = ListBuffer[Stamp]()
  override def toString = "(" + " InnerNode" + d + ")"
}


// TODO: never used
/*
 * An event automatically triggered by the framework.
 */
class InnerEventNode[T](d: DepHolder) extends EventNode[T] with Dependent {

  level = d.level + 1 // For glitch freedom
  d.addDependent(this) // To be notified in the future
  dependOn += d

  var storedVal: Any = _

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    notifyDependents(storedVal)
  }

  override def dependsOnchanged(change: Any,dep: DepHolder) = {
    storedVal = change
    ReactiveEngine.addToEvalQueue(this)
  }

  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = "(" + " InnerNode" + d + ")"
}



/*
 * Implementation of event disjunction
 */
class EventNodeOr[T](ev1: Event[_ <: T], ev2: Event[_ <: T]) extends EventNode[T] with Dependent {

  /*
   * The event is executed once and only once even if both sources fire in the
   * same propagation cycle. This is made sure by adding the node only once per cycle
   */
  var lastRoundAdded = 0

  level = (ev1.level max ev2.level) + 1 // For glitch freedom
  ev1.addDependent(this) // To be notified in the future
  ev2.addDependent(this)
  dependOn ++= List(ev1,ev2)

  var storedVal: Any = _

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    notifyDependents(storedVal)
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    val currentRound = TS.getCurrentTs.roundNum
    storedVal = change
    if(currentRound > lastRoundAdded) {
      lastRoundAdded = currentRound
      ReactiveEngine.addToEvalQueue(this)
    }
  }

  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = "(" + ev1 + " || " + ev2 + ")"
}


/*
 * Implementation of event conjunction
 */
class EventNodeAnd[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T)
                                                extends EventNode[T] with Dependent {

  // The round id of the last received event
  var lastRound = -1

  level = (ev1.level max ev2.level) + 1 // For glitch freedom
  ev1.addDependent(this) // To be notified in the future
  ev2.addDependent(this)
  dependOn ++= List(ev1,ev2)

  var storedValEv1: T1 = _
  var storedValEv2: T2 = _

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    notifyDependents(merge(storedValEv1,storedValEv2))
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {

    val round = TS.getCurrentTs match { case Stamp(round,_) => round }
    if(lastRound == round) {
      if (dep == ev1) storedValEv1 = change.asInstanceOf[T1]
      if (dep == ev2) storedValEv2 = change.asInstanceOf[T2]
      ReactiveEngine.addToEvalQueue(this)
    }
    if (dep == ev1) storedValEv1 = change.asInstanceOf[T1]
    if (dep == ev2) storedValEv2 = change.asInstanceOf[T2]
    lastRound = round
  }
  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = "(" + ev1 + " and " + ev2 + ")"

}





/*
 * Implements filtering event by a predicate
 */
class EventNodeFilter[T](ev: Event[T], f: T => Boolean) extends EventNode[T] with Dependent {

  level = ev.level + 1   // For glitch freedom
  ev.addDependent(this) // To be notified in the future
  dependOn += ev

  var storedVal: T = _

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    if(f(storedVal)) notifyDependents(storedVal)
  }

  override def dependsOnchanged(change: Any,dep: DepHolder) = {
    storedVal = change.asInstanceOf[T]
    ReactiveEngine.addToEvalQueue(this)
  }

  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = "(" + ev + " && <predicate>)"
}


/*
 * Implements transformation of event parameter
 */
class EventNodeMap[T, U](ev: Event[T], f: T => U)
  extends EventNode[U] with Dependent {

  level = ev.level + 1   // For glitch freedom
  ev.addDependent(this) // To be notified in the future
  dependOn += ev

  var storedVal: T = _

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    notifyDependents(f(storedVal))
  }

  override def dependsOnchanged(change: Any,dep: DepHolder) = {
    storedVal = change.asInstanceOf[T]
    ReactiveEngine.addToEvalQueue(this)
  }

  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = "(" + ev + " && <predicate>)"
}


/*
 * Implementation of event except
 */
class EventNodeExcept[T](accepted: Event[T], except: Event[T])
  extends EventNode[T] with Dependent {

  // The id of the last received event
  var lastTSAccepted = Stamp(-1,-1) // No round yet
  var lastTSExcept= Stamp(-1,-1)

  level = (accepted.level max except.level) + 1 // For glitch freedom
  accepted.addDependent(this) // To be notified in the future
  except.addDependent(this)
  dependOn ++= List(accepted,except)

  var storedVal: Any = _

  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    // Fire only if accepted is the one that fired and except did'fire
    if (lastTSAccepted.roundNum > lastTSExcept.roundNum ) notifyDependents(storedVal)
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (dep == accepted) {
      lastTSAccepted = TS.getCurrentTs
      storedVal = change
      ReactiveEngine.addToEvalQueue(this)
    }
    if (dep == except) lastTSExcept = TS.getCurrentTs
  }

  /* Testing */
  val timestamps: Buffer[Stamp] = ListBuffer()
  override def toString = "(" + accepted + " \\ " + except + ")"
}


object emptyevent extends Event[Nothing] {
  def +=(react: Nothing => Unit) { /* do nothing */ }
  def -=(react: Nothing => Unit) { /* do nothing */ }
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
    after((t, res))
    res
  }
}

object Observable {
  def apply[T,U](f: T => U) = new Observable(f)
}
