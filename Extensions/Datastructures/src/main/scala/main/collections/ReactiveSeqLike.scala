package main.collections

import rescala._

import scala.collection._
import scala.language.higherKinds

trait ReactiveSeqLike[A, ConcreteType[_]] extends ReactiveGenTraversableLike1[A, ConcreteType] {
	type InternalKind[B] <: SeqLike[B, InternalKind[B]]

	//Basic mutating functions
	val add = liftMutating1((xs: InternalType, x: A) => (xs :+ x).asInstanceOf[InternalType]) _
	val append = liftMutating1((xs: InternalType, ys: InternalType) => (xs ++ ys).asInstanceOf[InternalType]) _

	val update = liftMutating2(_.updated(_: Int, _: A).asInstanceOf[InternalKind[A]])_

	def apply(i: Signal[Int]) = liftPure1(_.apply(_: Int))(i)

	val size = liftPure0(_.size) _
	val head = liftPure0(_.head) _
	val last = liftPure0(_.last) _
	val tail = liftPure0(_.tail) _

	//aliases
	def +=(elem: Signal[A]): Unit = {
	    add(elem)
	}
}
