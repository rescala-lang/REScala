package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.List

class ReactiveList[A](list: Signal[List[A]]) extends ReactiveSeqLike[A, ReactiveList] {
	type InternalType[A] = List[A]
	
	override protected val collectionSignal: Var[Signal[InternalType[A]]] = Var(list)
	override protected def wrapSignal[B](signal: Signal[List[B]]) = new ReactiveList[B](signal)
	
	def this(set: List[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(List(vals: _*))
} 