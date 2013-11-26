package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A, ConcreteType[A]] {
	type InternalType[A] <: SetLike[A, InternalType[A]] with Set[A]
	
	protected val collectionSignal: Var[Signal[InternalType[A]]] 
	protected def wrapSignal(signal: Signal[InternalType[A]]): ConcreteType[A]
	
	def +=(elem: A) {
	    (+=)(Signal(elem))
	}
	
	def +=(elem: Signal[A]) {
	    //creates a chain of collections that remembers all changes and repeats them one by one if sth. changes
	    val signal = collectionSignal() 
	    collectionSignal() = SignalSynt[InternalType[A]](signal, elem) {
	        (x: SignalSynt[InternalType[A]]) => signal(x) + elem(x)
	    }
	}
	
	def -=(elem: A) {
	    (-=)(Signal(elem))
	}
	
	def -=(elem: Signal[A]) {
	    val signal = collectionSignal() 
	    collectionSignal() = SignalSynt[InternalType[A]](signal) {
	        (x: SignalSynt[InternalType[A]]) => signal(x) - elem(x)
	    }
	}
	
	protected def signal[B] = SignalSynt[B](collectionSignal) _ //why don't I need the current collectionSignal()? 
	
	
	def filter(f: A => Boolean): ConcreteType[A] = wrapSignal(signal[InternalType[A]] {
	    (x: SignalSynt[InternalType[A]]) => collectionSignal(x)(x).filter(f)
	})
	
	def contains(elem: A): Signal[Boolean] = signal[Boolean] {
	    (x: SignalSynt[Boolean]) => collectionSignal(x)(x).contains(elem)
	}
	//lazy val size = Signal(internalCollection()().size) //TODO: abstract these
}