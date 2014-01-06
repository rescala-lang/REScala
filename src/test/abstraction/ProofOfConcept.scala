package test.abstraction

import react._
import main.abstraction.SignalWrapper

class ProofOfConcept[T](list: List[T]) extends SignalWrapper {
	type InternalType = List[T]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(Var(list).toSignal)
	
	val length = fn0c(_.length)
	val head = fn0c(_.head)
	
	val duplicate = fn0m {
	    x => x ++ x
	}
}