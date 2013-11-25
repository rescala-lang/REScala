package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A] {
	type InternalType[A] <: SetLike[A, InternalType[A]] with Set[A]
	
	/** requires to be defined early! */
	protected val internalCollection: Var[InternalType[A]]
	protected val collectionSignal: Var[Signal[InternalType[A]]] = Var(internalCollection.toSignal)
	
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
	
	def signal[B] = SignalSynt[B](collectionSignal,collectionSignal()) _
	def contains(elem: A): Signal[Boolean] = signal[Boolean] {
	    (x: SignalSynt[Boolean]) => collectionSignal(x)(x).contains(elem)
	}
	//lazy val size = Signal(internalCollection()().size) //TODO: abstract these
}