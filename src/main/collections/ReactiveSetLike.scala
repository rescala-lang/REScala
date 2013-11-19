package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A] {
	type InternalType[A] <: SetLike[Signal[A], InternalType[A]] with Set[Signal[A]]
	
	protected val internalCollection: Var[InternalType[A]]
	def signal[B] = SignalSynt[B](internalCollection) _
	
	def +=(elem: A) {
	    internalCollection() += Signal(elem)
	}
	
	def +=(elem: Signal[A]) {
	    internalCollection() += elem
	}
	
	def -=(elem: A) {
	    internalCollection() -= Signal(elem)
	}
	
	
	def contains(elem: A): Signal[Boolean] = Signal (internalCollection().exists(_() == elem))
	
	def containsD(elem: A): Boolean = internalCollection().exists(_() == elem)
	
	//lazy val size = Signal(internalCollection()().size) //TODO: abstract these
}