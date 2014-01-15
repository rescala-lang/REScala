package main.collections

import react._
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable._
import scala.collection.GenTraversableOnce
import scala.collection.generic._
import scala.language.higherKinds
import main.abstraction._

// Not MapLike because MapLike is different from SeqLike and SetLike for some reason
trait ReactiveMap[A,B, ConcreteType[_,_]] extends SignalWrapper { 
	type InternalKind[C,D] = Map[C,D]
	type InternalType = InternalKind[A,B]
	
	val add = liftMutating1(_ + (_: (A,B))) _
	val remove = liftMutating1(_ - (_: A)) _
	val update = liftMutating2(_.updated(_: A, _: B)) _
	val size = liftPure0(_.size) _
	val get = liftPure1(_.get(_: A)) _
	
	def +=(elem: Signal[(A, B)]) {
	    add(elem)
	}
	
	def -=(key: Signal[A]) {
	    remove(key)
	}
	
	implicit val wrapping: SignalWrappable2[InternalKind, ConcreteType]
	
	val filter = liftPure1(_.filter(_: ((A,B)) => Boolean)) _ andThen wrap2
	
	//def map[C, That, WrappedThat](f: Signal[((A,B)) => C])(implicit cbf: CanBuildFrom[Map[A,B],C,That], wrapping: SignalWrappable0[That, WrappedThat]): Any =  
	//    (liftPure1(_.map(_: ((A,B)) => C)) _ andThen wrap0)(f)
	
	def map[C, D](f: Signal[((A,B)) => (C,D)]): ConcreteType[C,D] =  
	    (liftPure1(_.map(_: ((A,B)) => (C,D))(scala.collection.immutable.Map.canBuildFrom[C,D])) _ andThen wrap2)(f)
	    
	def flatMap[C, D](f: Signal[((A,B)) => GenTraversableOnce[(C,D)]]): ConcreteType[C,D] =  
	    (liftPure1(_.flatMap(_: ((A,B)) => GenTraversableOnce[(C,D)])(scala.collection.immutable.Map.canBuildFrom[C,D])) _ andThen wrap2)(f)    
	    
	def foldLeft[C](z: Signal[C])(op: Signal[(C, (A,B)) => C]) = 
	    liftPure2(_.foldLeft(_: C)(_: (C, (A,B)) => C))(z, op)
}