package main.collections

import react._
import scala.collection.immutable.ListSet

class ReactiveListSet[A](set: Signal[ListSet[A]]) extends ReactiveSetLike[A, ReactiveListSet] {
	override type InternalType[A] = ListSet[A]
	override protected val collectionSignal: Var[Signal[InternalType[A]]] = Var(set)
	override protected def wrapSignal[B](signal: Signal[ListSet[B]]) = new ReactiveListSet[B](signal)
	
	def this(set: ListSet[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(ListSet(vals: _*))
}