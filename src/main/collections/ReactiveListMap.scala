package main.collections

import react._
import scala.collection.immutable._
import main.abstraction._

class ReactiveListMap[A,B](map: Signal[Map[A,B]]) extends ReactiveMap[A,B, ReactiveListMap] {
	override protected val internalValue = Var(map)
	
	def this(map: ListMap[A,B]) = this(Var(map).toSignal)
	def this(pairs: (A,B)*) = this(ListMap(pairs:_*))
	
	implicit object wrapping extends SignalWrappable2[Map, ReactiveListMap] {
	    def wrap[A,B](unwrapped: Signal[Map[A,B]]): ReactiveListMap[A,B] = new ReactiveListMap(unwrapped)
	}
}