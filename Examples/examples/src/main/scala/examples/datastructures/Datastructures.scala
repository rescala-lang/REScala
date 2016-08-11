package examples.datastructures


import rescala._

import scala.collection.immutable.Queue

/**
  * A mutable Queue working with Signals
  * (wraps scala.collection.immutable.Queue)
  */
class SQueue[T] {
  val _queue = Var(Queue[T]())

  // some signals
  lazy val head = Signal {
    if (_queue().isEmpty) None
    else Some(_queue().head)
  }
  lazy val length = Signal {_queue().length}
  lazy val isEmpty = Signal {_queue().isEmpty}

  // methods mutating the state of the SQueue
  def enqueue(elem: T) = _queue() = _queue.now.enqueue(elem)
  def dequeue(): T = {
    val (first, tail): (T, Queue[T]) = _queue.now.dequeue
    _queue() = tail
    return first
  }
}

object SQueue {
  def apply[T](xs: T*): SQueue[T] = {
    val sq = new SQueue[T]
    sq._queue() = sq._queue.now.enqueue(xs.toList)
    return sq
  }
}


class SStack[T] {
  val _stack = Var(List[T]())

  // some signals
  lazy val top = Signal {
    if (_stack().isEmpty) None
    else Some(_stack().head)
  }
  lazy val length = Signal {_stack().size}
  lazy val isEmpty = Signal {_stack().isEmpty}

  // methods mutating the state of the SQueue
  def push(elem: T) = _stack.transform(elem :: _)
  def pop(): T = {
    val out :: rest = _stack.now
    _stack.set(rest)
    out
  }
}

object SStack {
  def apply[T](xs: T*): SStack[T] = {
    val ss = new SStack[T]
    ss._stack.transform(xs.toList ::: _)
    ss
  }
}
