package main.collections

import react._
import scala.collection.immutable.HashSet
import main.abstraction._

class ReactiveHashSet[A](set: Signal[HashSet[A]]) extends ReactiveSetLike[A, ReactiveHashSet] {
	override type InternalKind[B] = HashSet[B]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(set)
	
	def this(set: HashSet[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(HashSet(vals: _*))
	
	implicit object wrapping extends SignalWrappable1[HashSet, ReactiveHashSet] {
	    def wrap[T](unwrapped: Signal[HashSet[T]]): ReactiveHashSet[T] = new ReactiveHashSet(unwrapped)
	}
}