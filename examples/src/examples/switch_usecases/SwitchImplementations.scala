package examples.switch_usecases

import rescala._
import rescala.events._
import makro.SignalMacro.{ SignalM => Signal }

object SwitchImplementations {

  def snapshot[T, E](s: Signal[T])(e: Event[E]): Signal[T] = {
    val init = Signal(s.getValue)
    IFunctions.switch(e)(init)(new IFunctions.Factory[E, T] {
      override def apply(eVal: E): (Signal[T], IFunctions.Factory[E, T]) = {
        val freeze = s.getVal
        (Signal(freeze), this)
      }
    })
  }

  def fold[T, E](e: Event[E], init: T)(f: (T, E) => T): Signal[T] = {
    class StateFactory(val state: T) extends IFunctions.Factory[E, T] {
      override def apply(e: E): (Signal[T], IFunctions.Factory[E, T]) = {
        val folded = f(state, e)
        return (Signal { folded }, new StateFactory(folded))
      }
    }
    return IFunctions.switch(e)(Signal {init})(new StateFactory(init))
  }
  
   def foldMemoized[T, E](e: Event[E], init: T)(f: (T, E) => T): Signal[T] = {
     // very simple implementation: just add a mutable hashmap as a cache
     val cache = new collection.mutable.HashMap[(T, E), T]    
     class StateFactory(val state: T) extends IFunctions.Factory[E, T] {
      override def apply(e: E): (Signal[T], IFunctions.Factory[E, T]) = {
        val folded = cache.getOrElseUpdate((state, e), f(state, e))
        return (Signal { folded }, new StateFactory(folded))
      }
    }
    return IFunctions.switch(e)(Signal {init})(new StateFactory(init))
  }
   
   
   def foldMemoized2[T, E](e: Event[E], init: T)(f: (T, E) => T): Signal[T] = {
     // alternative: Use immutable map as cache
     class MemoizedStateFactory(val state: T, cache: Map[(T, E), T]) extends IFunctions.Factory[E, T] {
      override def apply(e: E): (Signal[T], IFunctions.Factory[E, T]) = {
        val folded = cache.getOrElse((state, e), f(state, e))
        return (Signal { folded }, new MemoizedStateFactory(folded, cache + (((state, e), folded))))
      }
    }
    return IFunctions.switch(e)(Signal {init})(new MemoizedStateFactory(init, Map.empty))
  }
  

}

object TestSwitchImplementations extends App {
  
  val e = new ImperativeEvent[Int]
  val sumNormal = e.fold(0)(_ + _)
  val sumFold1 = SwitchImplementations.fold(e, 0)(_ + _)
  val sumFold2 = SwitchImplementations.foldMemoized(e, 0)(_ + _)
  val sumFold3 = SwitchImplementations.foldMemoized2(e, 0)(_ + _)
  e(1)
  e(2)
  e(3) 
  assert(sumNormal.getVal == 6)
  assert(sumFold1.getVal == 6)
  assert(sumFold2.getVal == 6)
  assert(sumFold3.getVal == 6)
}