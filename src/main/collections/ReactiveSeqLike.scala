package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.immutable.Seq
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSeqLike[A] {
	type InternalType[A] <: SeqLike[A, InternalType[A]]
	
	protected val internalCollection: Var[InternalType[A]]
	
	//Basic mutating functions
	private type CBF = CanBuildFrom[InternalType[A], A, InternalType[A]]
	def append(values: A*)(implicit cbf: CBF) {
	    internalCollection() ++= (values)
	}
	
	def update(idx: Int, elem: A)(implicit cbf: CBF) {
	    internalCollection() = internalCollection().updated(idx, elem)
	}
	
	//Basic accessing functions
	def apply(idx: Int): Signal[A] = Signal(internalCollection()(idx))
	def apply(idx: Signal[Int]): Signal[A] = Signal(internalCollection()(idx()))
	
	lazy val length = Signal(internalCollection().length)
	lazy val head = Signal(internalCollection().head)
	lazy val last = Signal(internalCollection().last)
	lazy val tail = Signal(internalCollection().tail)
} 