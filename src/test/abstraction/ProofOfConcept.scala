package test.abstraction

import react._
import main.abstraction.SignalWrapper
import scala.collection.GenTraversableOnce

class ProofOfConcept[T](set: Set[T]) extends SignalWrapper {
	type InternalType = Set[T]
	override protected val internalValue: Var[Signal[InternalType]]  = Var(Var(set).toSignal)
	
	val size = liftPure0(_.size) _
	val head = liftPure0(_.head) _
	
	
	val duplicate = liftMutating0(xs => xs ++ xs) _
	
	val contains: Signal[T] => Signal[Boolean] = liftPure1(_.contains(_))
	
	val add = liftMutating1(_ + (_: T)) _
	
	def remove = liftMutating1(_ - (_: T)) _
	
	val filter = liftPure1(_.filter(_: T => Boolean)) _
	
	val filterSelf = liftMutating1(_.filter(_: T => Boolean)) _
	
	def map[B] = liftPure1(_.map(_: T => B)) _
	
	def flatMap[B] = liftPure1(_.flatMap(_: T => GenTraversableOnce[B])) _
	
	def fold[B >: T] = liftPure2(_.fold(_: B)(_: (B, B) => B)) _
	
	//aliases
	def +=(elem: Signal[T]) { add(elem) }
	def -=(elem: Signal[T]) { remove(elem)}
}