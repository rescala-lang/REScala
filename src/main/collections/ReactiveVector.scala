package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.Vector

class ReactiveVector[A](vector: Signal[Vector[A]]) extends ReactiveSeqLike[A, ReactiveVector] {
	type InternalType[A] = Vector[A]
	
	override protected val collectionSignal: Var[Signal[InternalType[A]]] = Var(vector)
	override protected def wrapSignal[B](signal: Signal[Vector[B]]) = new ReactiveVector[B](signal)
	
	def this(set: Vector[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(Vector(vals: _*))
} 