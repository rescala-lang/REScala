package main.collections

import react._
import scala.collection.immutable.ListSet
import main.abstraction._

class ReactiveListSet[A](set: Signal[ListSet[A]]) extends ReactiveSetLike[A, ReactiveListSet] {
	override type InternalKind[B] = ListSet[B]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(set)
	
	def this(set: ListSet[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(ListSet(vals: _*))
	
	implicit object wrapping extends SignalWrappable1[ListSet, ReactiveListSet] {
	    def wrap[T](unwrapped: Signal[ListSet[T]]): ReactiveListSet[T] = new ReactiveListSet(unwrapped)
	}
}