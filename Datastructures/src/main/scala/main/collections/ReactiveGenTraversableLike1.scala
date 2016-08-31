package main.collections

import main.abstraction._
import rescala._

import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveGenTraversableLike1[A, ConcreteType[_]] extends SignalWrapper {
	type InternalKind[B] <: GenTraversableLike[B, InternalKind[B]]
	type InternalType = InternalKind[A]
	implicit def wrapping[B]: SignalWrappable[InternalKind[B], ConcreteType[B]]

	type CBF[B] = CanBuildFrom[InternalKind[A], B, InternalKind[B]]


	val filter = liftPure1(_.filter(_: A => Boolean)) _ andThen wrap


	def map[B: CBF](f: Signal[A => B]): ConcreteType[B] =
	    (liftPure1[A => B, InternalKind[B]](_.map[B, InternalKind[B]](_: A => B)) _ andThen wrap)(f)

	def flatMap[B: CBF](f: Signal[A => GenTraversableOnce[B]]): ConcreteType[B] =
	    (liftPure1(_.flatMap[B, InternalKind[B]](_: A => GenTraversableOnce[B]))_ andThen wrap)(f)

	def foldLeft[B](z: Signal[B])(op: Signal[(B, A) => B]) = liftPure2(_.foldLeft(_: B)(_: (B, A) => B))(z, op)

}
