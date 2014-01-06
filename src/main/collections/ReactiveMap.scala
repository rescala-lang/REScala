package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable._
import scala.collection.GenTraversableOnce
import scala.collection.generic._
import scala.language.higherKinds

// Not MapLike because MapLike is different from SeqLike and SetLike for some reason
trait ReactiveMap[A,B, ConcreteType[_,_]] { 
	type InternalType[A,B] <: Map[A, B]
	protected val collectionSignal: Var[Signal[Map[A,B]]] 
	protected def wrapSignal(signal: Signal[Map[A,B]]): ConcreteType[A,B]
	
	def +=(elem: (A,B)) {
	    +=(Signal(elem))
	}
	
	def +=(elem: Signal[(A,B)]) {
	    val signal = collectionSignal() 
		collectionSignal() = SignalSynt[Map[A,B]](signal, elem) {
	        (x: SignalSynt[Map[A,B]]) => signal(x) + elem(x)
	    }
	}
	
	def update(key: Signal[A], value: Signal[B]) {
	    val signal = collectionSignal() 
		collectionSignal() = SignalSynt[Map[A,B]](signal, key, value) {
	        (x: SignalSynt[Map[A,B]]) => signal(x).updated(key(x), value(x))
	    }
	}
	
	def -=(key: Signal[A]) {
	    val signal = collectionSignal() 
		collectionSignal() = SignalSynt[Map[A,B]](signal, key) {
	        (x: SignalSynt[Map[A,B]]) => signal(x) - key(x)
	    }
	}
	
	lazy val size = Signal(collectionSignal()().size)
	def get(key: A): Signal[Option[B]] = Signal(collectionSignal()().get(key))
	def get(key: Signal[A]): Signal[Option[B]] = Signal(collectionSignal()().get(key()))
	
	private def wrapHigherOrderFunction[C](hf: Map[A,B] => (((A,B)) => C) => Map[A,B])(f: Signal[((A,B)) => C]) = SignalSynt[Map[A,B]] {
	    (x: SignalSynt[Map[A,B]]) => hf(collectionSignal(x)(x))(f(x))
	}
	val filter = wrapHigherOrderFunction[Boolean](_.filter)_ andThen wrapSignal
	
	def map(f: Signal[((A,B)) => B])(implicit CBT: CanBuildFrom[Map[A,B],B,Map[A,B]]): ConcreteType[A,B] = 
	    (wrapHigherOrderFunction(_.map[B, Map[A,B]])_ andThen wrapSignal)(f)
	
	def flatMap(f: Signal[((A,B)) => GenTraversableOnce[B]])(implicit CBT: CanBuildFrom[Map[A,B],B,Map[A,B]]) = 
	    (wrapHigherOrderFunction(_.flatMap[B, Map[A,B]])_ andThen wrapSignal)(f)
	
	def foldLeft(z: B)(f: Signal[(B, (A,B)) => B]): Signal[B] = foldLeft(Signal(z))(f)
	    
	def foldLeft(z: Signal[B])(f: Signal[(B, (A,B)) => B]): Signal[B] = SignalSynt[B](f,z) {
	   (x: SignalSynt[B]) => collectionSignal(x)(x).foldLeft[B](z(x))(f(x))
	}
}