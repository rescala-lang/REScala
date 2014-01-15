package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.List
import main.abstraction._

class ReactiveList[A](list: Signal[List[A]]) extends ReactiveSeqLike[A, ReactiveList] {
	override type InternalKind[B] = List[B]
	
	override protected val internalValue = Var(list)
	
	def this(set: List[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(List(vals: _*))
	
	implicit def wrapping[B] = new SignalWrappable[List[B], ReactiveList[B]] {
	    def wrap(unwrapped: Signal[List[B]]): ReactiveList[B] = new ReactiveList(unwrapped)
	}
} 
