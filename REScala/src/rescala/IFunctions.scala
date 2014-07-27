package rescala

import rescala.events._
import rescala.log._
import scala.collection.mutable.ListBuffer
import scala.collection.LinearSeq

object IFunctions {

  /** folds events with a given fold function to create a Signal */
  def fold[T, A](e: Event[T], init: A)(f: (A, T) => A): Signal[A] = new FoldedSignal(e, init, f)

  /** Iterates a value on the occurrence of the event. */
  def iterate[A](e: Event[_], init: A)(f: A => A): Signal[A] = fold(e, init)((acc, _) => f(acc))

  /**
   * Counts the occurrences of the event. Starts from 0, when the event has never been
   *  fired yet. The argument of the event is simply discarded.
   */
  def count(e: Event[_]): Signal[Int] = fold(e, 0)((acc, _) => acc + 1)

  /**
   * Calls f on each occurrence of event e, setting the Signal to the generated value.
   *  The initial signal is obtained by f(init)
   */
  def set[F, G](e: Event[F], init: F)(f: F => G): Signal[G] = fold(e, f(init))((_, v) => f(v))

  /** calls factory on each occurrence of event e, resetting the Signal to a newly generated one */
  def reset[T, A](e: Event[T], init: T)(factory: (T) => Signal[A]): Signal[A] = {
    val ref: Signal[Signal[A]] = set(e, init)(factory)
    SignalSynt { s: SignalSynt[A] => ref(s)(s) } // cannot express without high order signals
  }

  /** returns a signal holding the latest value of the event. */
  def latest[T](e: Event[T], init: T): Signal[T] = IFunctions.fold(e, init)((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  def latestOption[T](e: Event[T]): Signal[Option[T]] = latest(e.map((x: T) => Some(x)), None)

  /** collects events resulting in a variable holding a list of all values. */
  def list[T](e: Event[T]): Signal[List[T]] = fold(e, List[T]())((acc, v) => v :: acc)

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last[T](e: Event[T], n: Int): Signal[LinearSeq[T]] =
    fold(e, new collection.mutable.Queue[T]) {
      (acc: collection.mutable.Queue[T], v: T) =>
        if (acc.length >= n) acc.dequeue()
        //v +=: acc // (prepend)
        acc += v // (append)
        acc
    }

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  def snapshot[V](e: Event[_], s: Signal[V]): Signal[V] = fold(e, s.get)((_, _) => s.get)

  /**
   * Switch to a signal once, on the occurrence of event e. Initially the
   *  return value is set to the original signal. When the event fires,
   *  the result is a constant signal whose value is the value of the event.
   */
  def switchTo[T](e: Event[T], original: Signal[T]): Signal[T] = {
    val latest = latestOption(e)
    SignalSynt[T](latest, original) { s =>
      latest(s) match {
        case None => original(s)
        case Some(x) => x
      }
    }
  }

  /** Switch to a new Signal once, on the occurrence of event e. */
  def switchOnce[T](e: Event[_], original: Signal[T], newSignal: Signal[T]): Signal[T] = {
    val latest = latestOption(e)
    SignalSynt[T](latest, original, newSignal) { s =>
      latest(s) match {
        case None => original(s)
        case Some(_) => newSignal(s)
      }
    }
  }

  /** Switch back and forth between two signals on occurrence of event e */
  def toggle[T](e: Event[_], a: Signal[T], b: Signal[T]): Signal[T] = {
    val switched: Signal[Boolean] = iterate(e, false) { !_ }
    SignalSynt[T](switched, a, b) { s => if (switched(s)) b(s) else a(s) }
  }

  /** Like latest, but delays the value of the resulting signal by n occurrences */
  def delay[T](e: Event[T], init: T, n: Int): Signal[T] = {
    val history: Signal[LinearSeq[T]] = last(e, n + 1)
    SignalSynt[T](history) { s =>
      val h = history(s)
      if (h.size <= n) init else h.head
    }
  }

  /** Delays this signal by n occurrences */
  def delay[T](signal: Signal[T], n: Int): Signal[T] = delay(signal.changed, signal.get, n)

  /** lifts a function A => B to work on reactives */
  def lift[A, B](f: A => B): (Signal[A] => Signal[B]) = a => a.map(f)

  
  /** Generates a signal from an event occurrence */
  trait Factory[E, A] {
    def apply(eventValue: E): (Signal[A], Factory[E, A])
  }

  /** Generates a signal from an initial signal and a factory for subsequent ones */
  def switch[T, A](e: Event[T])(init: Signal[A])(factory: Factory[T, A]): Signal[A] =
    new SwitchedSignal(e, init, factory)
  
  /** Unwraps an event wrapped in a signal */
  def unwrap[T](wrappedEvent: Signal[Event[T]]): Event[T] = 
    new WrappedEvent(wrappedEvent)
}

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)
  extends DependentSignal[T] {

  // The value of this signal
  private[this] var currentValue: T = init

  // The cached value of the last occurence of e
  private[this] var lastEvent: E = _

  private[this] var inQueue = false

  def get = currentValue

  // The only dependant is e
  addDependOn(e)
  e.addDependent(this)
  this.level = e.level + 1

  def triggerReevaluation() = reEvaluate()

  def reEvaluate(): T = {
    ReactiveEngine.log.nodeEvaluationStarted(this)
    inQueue = false

    val hashBefore = if (currentValue == null) 0 else currentValue.hashCode
    val tmp = f(currentValue, lastEvent)
    val hashAfter = if (tmp == null) 0 else tmp.hashCode
    // support mutable values by using hashValue rather than ==
    if (hashAfter != hashBefore || currentValue != tmp) {
      currentValue = tmp
      timestamps += TS.newTs // Testing
      notifyDependents(currentValue)
    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      timestamps += TS.newTs // Testing
    }
    ReactiveEngine.log.nodeEvaluationEnded(this)
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
}

class SwitchedSignal[+T, +E](e: Event[E], init: Signal[T], factory: IFunctions.Factory[E, T])
  extends DependentSignal[T] {

  // The "inner" signal
  private[this] var currentSignal: Signal[T] = init
  // the current factory being called on the next occurence of e
  private[this] var currentFactory: IFunctions.Factory[E, T] = factory

  private[this] var inQueue = false

  def get = currentSignal.get

  private def removeInner(s: Signal[_]) {
    removeDependOn(s)
    s.removeDependent(this)
  }

  private def addInner(s: Signal[_]) {
    addDependOn(s)
    s.addDependent(this)
    this.level = math.max(e.level, s.level) + 1
  }

  // Switched signal depends on event and the current signal
  addDependOn(e)
  e.addDependent(this)
  addInner(currentSignal)

  def triggerReevaluation() = reEvaluate()

  def reEvaluate(): T = {
    ReactiveEngine.log.nodeEvaluationStarted(this)
    inQueue = false

    val inner = currentSignal.reEvaluate()
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
      val value = reEvaluate()
      notifyDependents(value)
    } else {
    }

    if (!inQueue) {
      inQueue = true
      ReactiveEngine.addToEvalQueue(this)
    }
  }
}
