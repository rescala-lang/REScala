package main.collections

import rescala._
import scala.collection.immutable.HashSet
import main.abstraction._

class ReactiveHashSet[A](set: Signal[HashSet[A]]) extends ReactiveSetLike[A, ReactiveHashSet] {
	override type InternalKind[B] = HashSet[B]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(set)
	
	def this(set: HashSet[A]) = this(Var(set))
	def this(vals: A*) = this(HashSet(vals: _*))
	
	implicit def wrapping[B] = new SignalWrappable[HashSet[B], ReactiveHashSet[B]] {
	    def wrap(unwrapped: Signal[HashSet[B]]): ReactiveHashSet[B] = new ReactiveHashSet(unwrapped)
	}
}