package rescala.core

import rescala.core.Node.InDep

import scala.language.higherKinds


trait Struct { type State[P, S <: Struct] }

/** A reactive value is something that can be reevaluated
  *
  * @tparam S Defines the structure of the internal state, as used by the propagation engine.
  */
trait Node[S <: Struct] {
  type Value

  /** Internal state of this reactive, managed by the propagation engine */
  protected[rescala] def state: S#State[Value, S]
}

object Node {
  type InDep[S <: Struct] = ReadableReactive[_, S]
  type OutDep[S <: Struct] = Reactive[S]
}

/** A reactive value is something that can be reevaluated
  *
  * @tparam S Defines the structure of the internal state, as used by the propagation engine.
  */
trait Reactive[S <: Struct] extends Node[S] {
  protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[InDep[S]]): ReevaluationResult[S]
}


/**
  * A reactive that user computations can read values from (e.g., before, after, etc.)
  *
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait ReadableReactive[+P, S <: Struct] extends Node[S] {
  override type Value <: P
}

/**
  * A reactive that the scheduler can write values on (i.e., reevOut)
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait WriteableReactive[-P, S <: Struct] extends Node[S] {
  override type Value >: P
}

/**
  * A base implementation for all reactives, tying together the Readable interface for user computations and the Writeable interface for schedulers.
  */
trait ReadWriteReactive[P, S <: Struct] extends ReadableReactive[P, S] with WriteableReactive[P, S]{
  override type Value = P
}

/**
  * A base implementation for all reactives, tying together the Readable interface for user computations and the Writeable interface for schedulers.
  */
abstract class Base[P, S <: Struct](initialState: S#State[Pulse[P], S], rename: REName) extends RENamed(rename) with ReadWriteReactive[Pulse[P], S] with Reactive[S] {
  final override protected[rescala] def state: S#State[Pulse[P], S] = initialState
}
