package main.collections

import rescala._

import scala.collection.immutable.Vector
import main.abstraction._

class ReactiveVector[A](Vector: Signal[Vector[A]]) extends ReactiveSeqLike[A, ReactiveVector] {
	override type InternalKind[B] = Vector[B]

	override protected val internalValue = Var(Vector)

	def this(set: Vector[A]) = this(Var(set))
	def this(vals: A*) = this(Vector(vals: _*))

	implicit def wrapping[B] = new SignalWrappable[Vector[B], ReactiveVector[B]] {
	    def wrap(unwrapped: Signal[Vector[B]]): ReactiveVector[B] = new ReactiveVector(unwrapped)
	}
}
