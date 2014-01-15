package main.collections

import react._
import scala.collection.immutable.ListSet
import main.abstraction._

class ReactiveListSet[A](set: Signal[ListSet[A]]) extends ReactiveSetLike[A, ReactiveListSet] {
	override type InternalKind[B] = ListSet[B]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(set)
	
	def this(set: ListSet[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(ListSet(vals: _*))
	
	implicit def wrapping[B] = new SignalWrappable[ListSet[B], ReactiveListSet[B]] {
	    def wrap(unwrapped: Signal[ListSet[B]]): ReactiveListSet[B] = new ReactiveListSet(unwrapped)
	}
}