package main.abstraction

import react._
import macro.SignalMacro.{SignalM => Signal}

abstract class SignalWrapper {
	type InternalType;
	
	protected val internalValue: Var[Signal[InternalType]] 
	
	
	protected def fn0c[ResultT](f: (InternalType) => ResultT): () => Signal[ResultT] = 
	    () => Signal(f(internalValue()()))
	    
	protected def fn0m(f: (InternalType) => InternalType): () => Unit = () => {
	    val signal = internalValue()
	    internalValue() = Signal(f(signal()))
	}
	    
	protected def fn0[ResultT](mutateF: (InternalType) => InternalType)(resultF: (InternalType) => ResultT): () => Signal[ResultT] = {
	    fn0m(mutateF)
	    fn0c(resultF)
	}
	
	
	
}