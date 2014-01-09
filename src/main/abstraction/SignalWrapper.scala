package main.abstraction

import react._
import macro.SignalMacro.{SignalM => Signal}

abstract class SignalWrapper {
	type InternalType;
	
	protected val internalValue: Var[Signal[InternalType]] 
	
	/*
	protected def fnc[TupleT, ResultT](f: InternalType => TupleT => ResultT): TupleT => Signal[ResultT] = 
	    (params: TupleT) => Signal(f(internalValue()())(params))
	    
	protected def fnm[TupleT](f: InternalType => TupleT => InternalType): TupleT => Unit = 
	    params => {
		    val signal = internalValue()
		    internalValue() = Signal(f(signal())(params))
	    }
	*/
	    
	protected def fn0c[ResultT](f: InternalType => ResultT)(): Signal[ResultT] = 
	    Signal(f(internalValue()()))
	    
	protected def fn0m(f: InternalType => InternalType)() {
	    val signal = internalValue()
	    internalValue() = Signal(f(signal()))
	}
	    
	protected def fn0[ResultT](mutateF: (InternalType) => InternalType)(resultF: (InternalType) => ResultT)(): Signal[ResultT] = {
	    fn0m(mutateF)
	    fn0c(resultF)
	}
	
	protected def fn1c[Param1T, ResultT](f: (InternalType, Param1T) => ResultT)(p1: Signal[Param1T]): Signal[ResultT] =
	    Signal(f(internalValue()(), p1()))
	    
	protected def fn1m[Param1T, ResultT](f: (InternalType, Param1T) => InternalType)(p1: Signal[Param1T]) {
	    val signal = internalValue()
	    internalValue() = Signal(f(signal(), p1()))
	}
	
	protected def fn2c[Param1T, Param2T, ResultT](f: (InternalType, Param1T, Param2T) => ResultT)(p1: Signal[Param1T], p2: Signal[Param2T]): Signal[ResultT] =
	    Signal(f(internalValue()(), p1(), p2()))
	    
	protected def fn2m[Param1T, Param2T, ResultT](f: (InternalType, Param1T, Param2T) => InternalType)(p1: Signal[Param1T], p2: Signal[Param2T]) {
	    val signal = internalValue()
	    internalValue() = Signal(f(signal(), p1(), p2()))
	}
	
	
}