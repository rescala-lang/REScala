package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveGenTraversableLike[A, ConcreteType[_]] {
	type InternalType[A] <: GenTraversableLike[A, InternalType[A]]
	
	type CBF[B] = CanBuildFrom[InternalType[A], B, InternalType[B]]
	
	protected val collectionSignal: Var[Signal[InternalType[A]]] 
	protected def wrapSignal[B](signal: Signal[InternalType[B]]): ConcreteType[B]
	
	protected def signal[B] = SignalSynt[B](collectionSignal) _ //why don't I need the current collectionSignal()?
	protected def wrapHigherOrderFunction[B, C, F](hf: InternalType[A] => F => C)(f: Signal[F]) = signal[C] {
	    (x: SignalSynt[C]) => hf(collectionSignal(x)(x))(f(x))
	}
	
	val filter = wrapHigherOrderFunction(_.filter)_ andThen wrapSignal
	
	def map[B: CBF](f: Signal[A => B]): ConcreteType[B] = 
	    (wrapHigherOrderFunction(_.map[B, InternalType[B]])_ andThen wrapSignal)(f)
	    
	def flatMap[B: CBF](f: Signal[A => GenTraversableOnce[B]]) = 
	    (wrapHigherOrderFunction(_.flatMap[B, InternalType[B]])_ andThen wrapSignal)(f)
	
	def foldLeft[B](z: B)(f: Signal[(B, A) => B]): Signal[B] = foldLeft(Signal(z))(f)
	    
	def foldLeft[B](z: Signal[B])(f: Signal[(B, A) => B]): Signal[B] = signal[B] {
	   (x: SignalSynt[B]) => collectionSignal(x)(x).foldLeft[B](z(x))(f(x))
	}
}