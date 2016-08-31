package main.collections

import rescala._

import scala.collection._
import scala.language.higherKinds

trait ReactiveSetLike[A, ConcreteType[_]] extends ReactiveGenTraversableLike1[A, ConcreteType] {
	type InternalKind[B] <: SetLike[B, InternalKind[B]] with Set[B]

	val add = liftMutating1(_ + (_: A)) _
	val remove = liftMutating1(_ - (_: A)) _

	val contains = liftPure1(_.contains(_: A)) _
	val size = liftPure0(_.size) _

	//aliases
	def +=(elem: Signal[A]): Unit = {
	    add(elem)
	}

	def -=(elem: Signal[A]): Unit = {
	    remove(elem)
	}
}
