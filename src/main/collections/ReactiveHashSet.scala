package main.collections

import react._
import scala.collection.immutable.HashSet

class ReactiveHashSet[A](set: HashSet[A]) extends ReactiveSetLike[A] {
	type InternalType[A] = HashSet[A]
	
	protected val internalCollection: Var[Signal[InternalType[A]]] = Var(Var(set).toSignal)
	
	def this(vals: A*) = this(HashSet(vals: _*))
}