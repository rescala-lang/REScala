package main.collections

import react._
import scala.collection.immutable.HashSet

class ReactiveHashSet[A](set: Signal[HashSet[A]]) extends ReactiveSetLike[A, ReactiveHashSet] {
	override type InternalType[A] = HashSet[A]
	override protected val collectionSignal: Var[Signal[InternalType[A]]] = Var(set)
	override protected def wrapSignal(signal: Signal[HashSet[A]]) = new ReactiveHashSet[A](signal)
	
	def this(set: HashSet[A]) = this(Var(set).toSignal)
	def this(vals: A*) = this(HashSet(vals: _*))
}