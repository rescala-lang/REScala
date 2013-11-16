package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSetLike[A] {
	type InternalType[A] <: SetLike[A, InternalType[A]] with Set[A]
	
	protected val internalCollection: Var[InternalType[A]]
	
	def +=(elem: A) {
	    internalCollection() += elem
	}
	
	def -=(elem: A) {
	    internalCollection() -= elem
	}
	
	def contains(elem: A): Signal[Boolean] = Signal {internalCollection().contains(elem)}
	lazy val size = Signal(internalCollection().size) //TODO: abstract these
}