package test.abstraction

import react._
import main.abstraction.SignalWrapper
import scala.collection.GenTraversableOnce

class ProofOfConcept[T](set: Set[T]) extends SignalWrapper {
	type InternalType = Set[T]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(Var(set).toSignal)
	
	val size: () => Signal[Int] = fn0c(_.size)
	val head: () => Signal[T] = fn0c(_.head)
	
	
	val duplicate: () => Unit = fn0m {
	    xs => xs ++ xs
	}
	
	val contains: Signal[T] => Signal[Boolean] = fn1c(_.contains(_: T))
	
	val add: Signal[T] => Unit = fn1m {
	    (xs,x1: T) => xs + x1
	}
	
	val remove: Signal[T] => Unit = fn1m {
	    (xs,x1: T) => xs - x1
	}
	
	val filter: Signal[T => Boolean] => Signal[Set[T]] = fn1c {
	    (xs, f: T => Boolean) => xs.filter(f)
	}
	
	val filterSelf: Signal[T => Boolean] => Unit = fn1m {
	    (xs, f: T => Boolean) => xs.filter(f)
	}
	
	def map[B]: Signal[T => B] => Signal[Set[B]] = fn1c {
	    (xs, f: T => B) => xs.map(f)
	}
	
	def flatMap[B]: Signal[T => GenTraversableOnce[B]] => Signal[Set[B]] = fn1c {
	    (xs, f: T => GenTraversableOnce[B]) => xs.flatMap(f)
	}
	
	def fold[B >: T]: (Signal[(B, B) => B], Signal[B]) => Signal[B] = fn2c {
	    (xs, f: (B, B) => B, z: B) => xs.fold(z)(f)
	}
	
	//aliases
	def +=(elem: Signal[T]) { add(elem) }
	def -=(elem: Signal[T]) { remove(elem)}
}