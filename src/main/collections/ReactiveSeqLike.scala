package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSeqLike[A, ConcreteType[_]] extends ReactiveGenTraversableLike[A, ConcreteType] {
	type InternalType[A] <: SeqLike[A, InternalType[A]]
	
	
	//Basic mutating functions
	def append(firstValue: Signal[A], values: Signal[A]*)(implicit cbf: CBF[A]) {
	    val signal = collectionSignal() 
	    collectionSignal() = SignalSynt[InternalType[A]](signal :: firstValue :: values.toList) {
	        (x: SignalSynt[InternalType[A]]) => signal(x) ++ (firstValue(x) +: values.map(_(x)))
	    }
	}
	
	def append(firstValue: A, values: A*)(implicit cbf: CBF[A]) {
	    append(Signal(firstValue), values.map(Var(_).toSignal):_*)
	}
	
	def update(idx: Signal[Int], elem: Signal[A])(implicit cbf: CBF[A]) {
		val signal = collectionSignal() 
	    collectionSignal() = SignalSynt[InternalType[A]](signal, idx, elem) {
	        (x: SignalSynt[InternalType[A]]) => signal(x).updated(idx(x), elem(x))
	    }
	}
	def update(idx: Int, elem: Signal[A])(implicit cbf: CBF[A]) { update(Var(idx).toSignal, elem) }
	def update(idx: Signal[Int], elem: A)(implicit cbf: CBF[A]) { update(idx, Var(elem).toSignal) }
	def update(idx: Int, elem: A)(implicit cbf: CBF[A]) { update(idx, Var(elem).toSignal) }
	
	//Basic accessing functions
	def apply(idx: Int): Signal[A] = Signal(collectionSignal()()(idx))
	def apply(idx: Signal[Int]): Signal[A] = Signal(collectionSignal()()(idx()))
	
	lazy val length = Signal(collectionSignal()().length)
	lazy val head = Signal(collectionSignal()().head)
	lazy val last = Signal(collectionSignal()().last)
	lazy val tail = Signal(collectionSignal()().tail)
} 