package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.List

class ReactiveList[A](list: List[A]) extends ReactiveSeqLike[A] {
	type InternalType[A] = List[A]
	protected val internalCollection = Var(list)
	
	def this(vals: A*) = this(List(vals: _*))
} 