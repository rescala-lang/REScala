package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection._
import scala.collection.generic._
import scala.language.higherKinds

trait ReactiveSeqLike[A, ConcreteType[_]] extends ReactiveGenTraversableLike1[A, ConcreteType] {
	type InternalKind[A] <: SeqLike[A, InternalKind[A]]
	
	
	//Basic mutating functions
	
	val add = liftMutating1((xs: InternalType, x: A) => (xs :+ x).asInstanceOf[InternalType]) _
	
	//def append(values: ReactiveSeqLike[A, ConcreteType])(implicit cbf: CBF[A]) =
	//    liftMutating2(appendImpl _)(firstValue, Signal(values.map(_.apply))
	
	val update = liftMutating2(_.updated(_: Int, _: A).asInstanceOf[InternalKind[A]])_
	
	def apply(i: Signal[Int]) = liftPure1(_.apply(_: Int))(i)
	
	val size = liftPure0(_.size) _
	val head = liftPure0(_.head) _
	val last = liftPure0(_.last) _
	val tail = liftPure0(_.tail) _
	
	//aliases
	def +=(elem: Signal[A]) {
	    add(elem)
	}
} 