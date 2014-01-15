package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.Vector
import main.abstraction._

class ReactiveVector[A](Vector: Signal[Vector[A]]) extends ReactiveSeqLike[A, ReactiveVector] {
	override type InternalKind[B] = Vector[B]
	
	override protected val internalValue = Var(Vector)
	
	def this(set: Vector[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(Vector(vals: _*))
	
	implicit object wrapping extends SignalWrappable1[Vector, ReactiveVector] {
	    def wrap[T](unwrapped: Signal[Vector[T]]): ReactiveVector[T] = new ReactiveVector(unwrapped)
	}
} 