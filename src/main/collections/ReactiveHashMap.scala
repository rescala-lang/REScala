package main.collections

import react._
import scala.collection.immutable._

class ReactiveHashMap[A,B](map: HashMap[A,B]) extends ReactiveMap[A,B] {
	protected val internalCollection: Var[Map[A,B]] = Var(map)
	
	def this(pairs: (A,B)*) = this(HashMap(pairs:_*))
}