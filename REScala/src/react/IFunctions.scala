package react

import react.events._
import react.log._


object IFunctions {

  

  /** folds events with a given fold function to create a Signal */
  def fold[T,A](e: Event[T], init: A)(f: (A,T)=>A): Signal[A] = {
      val v: Var[A] = Var(init)
	  e += {(newVal: T) => v.setVal(f(v.getValue,newVal)) }
	  val result = StaticSignal(v){v.getValue}
	  ReactiveEngine.log log LogIFAttach(ReactiveEngine.log.node(result), ReactiveEngine.log.node(e)) // log the 'virtual' dependency
	  return result
  }

  /** Iterates a value on the occurrence of the event. */
  def iterate[A](e: Event[_], init: A)(f: A=>A): Signal[A] = fold(e,init)( (acc,_)=>f(acc) )
  
   /** Calls f on each occurrence of event e, setting the Signal to the generated value.
    *  The initial signal is obtained by f(init) 
    */
  def set[F,G](e: Event[F], init: F)(f: F=>G): Signal[G] = fold(e,f(init))((_,v) => f(v))

  
  /*
  def reset[T, A](e: Event[T], init: T)(factory: (T) => Signal[A]): Signal[A] = {
    val ref: Signal[Signal[A]] = set(e, init)(factory)
    Signal{ ref()() }
  }
  */
  /** calls factory on each occurrence of event e, resetting the Signal to a newly generated one */
  def reset[T, A](e: Event[T], init: T)(factory: (T) => Signal[A]): Signal[A] = {
    val ref: Signal[Signal[A]] = set(e, init)(factory)
    SignalSynt{s: SignalSynt[A]=> ref(s)(s) } // cannot express without high order signals
  }
  
  /** returns a signal holding the latest value of the event. */
  def latest[T](e : Event[T], init : T): Signal[T] = IFunctions.fold(e, init)((_,v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  def latestOption[T](e : Event[T]) : Signal[Option[T]] = latest(e.map((x : T) => Some(x)), None)
   
  /** collects events resulting in a variable holding a list of all values. */
  def list[T](e : Event[T]): Signal[List[T]] = fold(e, List[T]())((acc, v) => v :: acc)
  
  /** Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last[T](e : Event[T], n : Int) : Signal[List[T]] = fold(e, List[T]())((acc, v) => (v :: acc).take(n)) 
  
  
  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  def snapshot[V](e : Event[_], s: Signal[V]): Signal[V] = fold(e, s.getVal)((_,_) => s.getVal)
   
  
   /** Switch to a signal once, on the occurrence of event e. Initially the 
    *  return value is set to the original signal. When the event fires,
    *  the result is a constant signal whose value is the value of the event. 
    */
   def switchTo[T](e : Event[T], original: Signal[T]): Signal[T] = {
    val latest = latestOption(e)
    StaticSignal(latest,original){ latest.getVal match {
      case None => original.getVal
      case Some(x) => x
    }}
  }
  
  /** Switch to a new Signal once, on the occurrence of event e. */
  def switchOnce[T](e: Event[_], original: Signal[T], newSignal: Signal[T]): Signal[T] = {
    val latest = latestOption(e)
    StaticSignal(latest,original,newSignal){ latest.getVal match {
      case None => original.getVal
      case Some(_) => newSignal.getVal
    }}
  }
  
  
  /** Switch back and forth between two signals on occurrence of event e */
  def toggle[T](e : Event[_], a: Signal[T], b: Signal[T]): Signal[T] = {
    val switched: Signal[Boolean] = iterate(e, false) {! _}
    StaticSignal(switched,a,b){ if(switched.getVal) b.getVal else a.getVal }
  }
  
  
  
  /** Like latest, but delays the value of the resulting signal by n occurrences */
  def delay[T](e: Event[T], init: T, n: Int): Signal[T] = {
    val history: Signal[List[T]] = last(e, n + 1)
    StaticSignal(history){
        val h = history.getVal
    	if(h.size <= n) init else h.last
    }
  }
  
  
  
  /** Delays this signal by n occurrences */
  def delay[T](signal: Signal[T], n: Int): Signal[T] = delay(signal.changed , signal.getVal, n)
 
   /** lifts a function A => B to work on reactives */
  def lift[A,B](f: A=>B): (Signal[A]=>Signal[B]) = (a => StaticSignal[B](a){ f(a.getVal) })
  


  // TODO: work in progress
  /** Generates a signal from an event occurrence */
  trait Factory[E, A] {
    def apply(eventValue : E) : (Signal[A], Factory[E,A])
  }
  
  /** Generates a signal from an initial signal and a factory for subsequent ones */
  def switch[T,A](e: Event[T])(init : Signal[A])(factory : Factory[T,A]) : Signal[A] = {
    
    val ref : Var[Signal[A]] = Var(init)
    val fac : Var[Factory[T,A]] = Var(factory)
    
    val handleSignal = {(eventValue : T) =>
      val currentFactory = fac()
      val (newSignal, newFactory) = currentFactory(eventValue)
      fac() = newFactory
      ref() = newSignal
    }
    
    e += handleSignal
    return SignalSynt{s: SignalSynt[A]=> ref(s)(s) } // cannot express without high order
  }


  
}


