package main.collections

import react._
import scala.collection.immutable._
import main.abstraction._

class ReactiveHashMap[A,B](map: Signal[Map[A,B]]) extends ReactiveMap[A,B, ReactiveHashMap] {
	override protected val internalValue = Var(map)
	
	def this(map: HashMap[A,B]) = this(Var(map).toSignal)
	def this(pairs: (A,B)*) = this(HashMap(pairs:_*))
	
	implicit object wrapping extends SignalWrappable2[Map, ReactiveHashMap] {
	    def wrap[A,B](unwrapped: Signal[Map[A,B]]): ReactiveHashMap[A,B] = new ReactiveHashMap(unwrapped)
	}
}
