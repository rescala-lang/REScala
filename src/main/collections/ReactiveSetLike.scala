package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A] {
	type InternalType[A] <: SetLike[A, InternalType[A]] with Set[A]
	
	protected val internalCollection: Var[Signal[InternalType[A]]]
	def signal[B] = SignalSynt[B](internalCollection) _
	
	def +=(elem: A) {
	    internalCollection() = SignalSynt[InternalType[A]](internalCollection) {
	        (x: SignalSynt[InternalType[A]]) => internalCollection(x)(x) + elem
	    }
	}
	
	def +=(elem: Signal[A]) {
	    internalCollection() = SignalSynt[InternalType[A]](internalCollection) {
	        (x: SignalSynt[InternalType[A]]) => internalCollection(x)(x) + elem(x)
	    }
	}
	
	def -=(elem: A) {
	    internalCollection() = SignalSynt[InternalType[A]](internalCollection) {
	        (x: SignalSynt[InternalType[A]]) => internalCollection(x)(x) - elem
	    }
	}
	
	def contains(elem: A): Signal[Boolean] = signal[Boolean] {
	    (x: SignalSynt[Boolean]) => internalCollection(x)(x).contains(elem)
	}
	//lazy val size = Signal(internalCollection()().size) //TODO: abstract these
}