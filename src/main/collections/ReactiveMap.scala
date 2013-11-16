package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveMap[A,B] { // Not MapLike because MapLike is different from SeqLike and SetLike for some reason
	type InternalType[A,B] <: Map[A, B]
	
	protected val internalCollection: Var[Map[A, B]] // As a result of the fact that this is not MapLike, we cannot use the concrete type
	
	def +=(elem: (A,B)) {
	    internalCollection() += elem
	}
	
	def update(key: A, value: B) {
	    internalCollection() = internalCollection().updated(key, value)
	}
	
	lazy val size = Signal(internalCollection().size)
	def get(key: A): Signal[Option[B]] = Signal(internalCollection().get(key))
	def get(key: Signal[A]): Signal[Option[B]] = Signal(internalCollection().get(key()))
}