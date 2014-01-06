package main.collections

import react._
import scala.collection.immutable._

class ReactiveHashMap[A,B](map: Signal[Map[A,B]]) extends ReactiveMap[A,B, ReactiveHashMap] {
	protected val collectionSignal: Var[Signal[Map[A,B]]] = Var(map)
	protected def wrapSignal(signal: Signal[Map[A,B]]): ReactiveHashMap[A,B] =
	    new ReactiveHashMap[A,B](signal)
	
	def this(map: HashMap[A,B]) = this(Var(map).toSignal)
	def this(pairs: (A,B)*) = this(HashMap(pairs:_*))
}