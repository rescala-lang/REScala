package main.collections

import react._
import scala.collection.immutable.HashSet

class ReactiveHashSet[A](set: HashSet[A]) extends {
	override protected val internalCollection: Var[HashSet[A]] = Var(set)
	} with ReactiveSetLike[A] {
	override type InternalType[A] = HashSet[A]
	
	def this(vals: A*) = this(HashSet(vals: _*))
}