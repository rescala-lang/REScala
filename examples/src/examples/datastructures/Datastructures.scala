package examples.datastructures



import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}
import scala.collection.immutable.Queue
import scala.collection.immutable.Stack

/**
 * A mutable Queue working with Signals 
 * (wraps scala.collection.immutable.Queue)
 */
class SQueue[T] {
	val _queue = Var(Queue[T]())
	
	// some signals
	lazy val head = Signal {
	  if(_queue().isEmpty) None 
	  else Some(_queue().head)
	}
	lazy val length = Signal {_queue().length}
	lazy val isEmpty = Signal { _queue().isEmpty }
	
	// methods mutating the state of the SQueue
	def enqueue(elem : T) = _queue() = _queue().enqueue(elem)
	def dequeue() : T = {
	  val (first : T, tail : Queue[T]) = _queue().dequeue
	  _queue() = tail
	  return first
	}
}

object SQueue {
  def apply[T](xs : T*) : SQueue[T] = {
    val sq = new SQueue[T]
    sq._queue() = sq._queue().enqueue(xs.toList)
    return sq
  }
}



class SStack[T] {
	val _stack =  Var(Stack[T]())
	
	// some signals
	lazy val top = Signal {
	  if(_stack().isEmpty) None 
	  else Some(_stack().top)
	}
	lazy val length = Signal {_stack().length}
	lazy val isEmpty = Signal { _stack().isEmpty }
	
	// methods mutating the state of the SQueue
	def push(elem : T) = _stack() = _stack().push(elem)
	def pop() : T = {
	  val (out : T, rest : Stack[T]) = _stack().pop2
	  _stack() = rest
	  return out
	}
}

object SStack {
  def apply[T](xs : T*) : SStack[T] = {
    val ss = new SStack[T]
    ss._stack() = ss._stack().pushAll(xs.toList)
    return ss
  }
}
