package main.collections

import main.abstraction._
import rescala._

import scala.collection.GenTraversableOnce
import scala.collection.generic._
import scala.collection.immutable._
import scala.language.higherKinds

// Not MapLike because MapLike is different from SeqLike and SetLike for some reason
trait ReactiveMap[A,B, ConcreteType[_,_]] extends SignalWrapper {
	type InternalKind[C,D] = Map[C,D]
	type InternalType = InternalKind[A,B]

	val add = liftMutating1(_ + (_: (A,B))) _
	val remove = liftMutating1(_ - (_: A)) _
	val update = liftMutating2(_.updated(_: A, _: B)) _
	val size: () => rescala.Signal[Int] = liftPure0(_.size) _
	val get = liftPure1(_.get(_: A)) _

	def +=(elem: Signal[(A, B)]): Unit = {
	    add(elem)
	}

	def -=(key: Signal[A]): Unit = {
	    remove(key)
	}

	private type SelfWrappable = SignalWrappable[InternalKind[A,B], ConcreteType[A,B]]

	def filter(f: Signal[((A,B)) => Boolean])(implicit ev: SelfWrappable): ConcreteType[A,B] =
	    (liftPure1(_.filter(_: ((A,B)) => Boolean)) _ andThen wrap)(f)

	def map[C, That, WrappedThat](f: Signal[((A, B)) => C])(implicit cbf: CanBuildFrom[Map[_,_], C, That], wrapping: SignalWrappable[That, WrappedThat]): WrappedThat =
	    (liftPure1(_.map(_: ((A,B)) => C)) _ andThen wrap)(f)

	def flatMap[C, That, WrappedThat](f: Signal[((A, B)) => GenTraversableOnce[C]])(implicit cbf: CanBuildFrom[Map[_,_], C, That], wrapping: SignalWrappable[That, WrappedThat]): WrappedThat =
	    (liftPure1(_.flatMap(_: ((A,B)) => GenTraversableOnce[C])) _ andThen wrap)(f)

	def foldLeft[C](z: Signal[C])(op: Signal[(C, (A,B)) => C]) =
	    liftPure2(_.foldLeft(_: C)(_: (C, (A,B)) => C))(z, op)
}
