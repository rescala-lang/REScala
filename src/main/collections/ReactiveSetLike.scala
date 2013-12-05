package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A, ConcreteType[A]] {
	type InternalType[A] <: SetLike[A, InternalType[A]] with Set[A]
	
	protected val collectionSignal: Var[Signal[InternalType[A]]] 
	protected def wrapSignal[B](signal: Signal[InternalType[B]]): ConcreteType[B]
	
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
	
	protected def signal[B] = SignalSynt[B](collectionSignal) _ //why don't I need the current collectionSignal()? 
	
	def wrapHigherOrderFunction[B, C, F](hf: InternalType[A] => F => C)(f: F) = signal[C] {
	    (x: SignalSynt[C]) => hf(collectionSignal(x)(x))(f)
	}
	
	
	val filter = wrapHigherOrderFunction(_.filter)_ andThen wrapSignal
	
	type CBF[B] = CanBuildFrom[InternalType[A], B, InternalType[B]]
	def map[B](f: A => B)(implicit cbf: CBF[B]): ConcreteType[B] = 
	    (wrapHigherOrderFunction(_.map[B, InternalType[B]])_ andThen wrapSignal)(f)
	    
	def flatMap[B](f: A => GenTraversableOnce[B])(implicit cbf: CBF[B]) = 
	    (wrapHigherOrderFunction(_.flatMap[B, InternalType[B]])_ andThen wrapSignal)(f)
	
	def foldLeft[B](z: B)(f: (B, A) => B): Signal[B] = foldLeft(Signal(z))(f)
	    
	def foldLeft[B](z: Signal[B])(f: (B, A) => B): Signal[B] = signal[B] {
	   (x: SignalSynt[B]) => collectionSignal(x)(x).foldLeft[B](z(x))(f)
	}
	
	
	def contains(elem: A): Signal[Boolean] = Signal(collectionSignal()().contains(elem))
	lazy val size = signal[Int]((x: SignalSynt[Int]) => collectionSignal(x)(x).size) 
}