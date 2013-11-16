package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.List

class ReactiveList[A](vals: A*) extends ReactiveSeqLike[A] {
	type InternalType[A] = List[A]
	
	protected val internalCollection = Var(List[A](vals: _*))
} 