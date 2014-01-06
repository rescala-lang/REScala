package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A, ConcreteType[_]] extends ReactiveGenTraversableLike1[A, ConcreteType] {
	type InternalType[A] <: SetLike[A, InternalType[A]] with Set[A]
	
	
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
	    collectionSignal() = Signal( signal() - elem() )
	    
	}
	
	
	def contains(elem: A): Signal[Boolean] = Signal(collectionSignal()().contains(elem))
	lazy val size = signal[Int]((x: SignalSynt[Int]) => collectionSignal(x)(x).size) 
}