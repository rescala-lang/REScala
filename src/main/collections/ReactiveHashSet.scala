package main.collections

import react._
import scala.collection.immutable.HashSet

class ReactiveHashSet[A](set: HashSet[Signal[A]]) extends ReactiveSetLike[A] {
	type InternalType[A] = HashSet[Signal[A]]
	
	protected val internalCollection = Var(set)
	
	def this(vals: A*) = this(HashSet(vals.map(Var(_).toSignal: Signal[A]): _*))
}