package main.collections

import react._
import scala.collection.immutable._

class ReactiveListMap[A,B](map: Signal[Map[A,B]]) extends ReactiveMap[A,B, ReactiveListMap] {
	protected val collectionSignal: Var[Signal[Map[A,B]]] = Var(map)
	protected def wrapSignal(signal: Signal[Map[A,B]]): ReactiveListMap[A,B] =
	    new ReactiveListMap[A,B](signal)
	
	def this(map: ListMap[A,B]) = this(Var(map).toSignal)
	def this(pairs: (A,B)*) = this(ListMap(pairs:_*))
}