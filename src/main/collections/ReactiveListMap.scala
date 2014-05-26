package main.collections

import rescala._
import scala.collection.immutable._
import main.abstraction._

class ReactiveListMap[A,B](map: Signal[Map[A,B]]) extends ReactiveMap[A,B, ReactiveListMap] {
	override protected val internalValue = Var(map)
	
	def this(map: ListMap[A,B]) = this(Var(map))
	def this(pairs: (A,B)*) = this(ListMap(pairs:_*))
	
	
}

object ReactiveListMap {
    implicit def wrapping[C,D] = new SignalWrappable[Map[C,D], ReactiveListMap[C,D]] {
	    def wrap(unwrapped: Signal[Map[C,D]]): ReactiveListMap[C,D] = new ReactiveListMap(unwrapped)
	}
}