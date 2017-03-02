package rescala.graph

import rescala.propagation.Turn

import scala.language.{existentials, higherKinds, implicitConversions}

/**
  * Wrapper that adds a level of indirection for classes having a struct type dependency.
 */
trait Struct {
  /**
    * Spore type defined by this struct
    *
    * @tparam P Pulse stored value type
    * @tparam R Reactive value type represented by the struct
    */
  type StructType[P, R] <: ReadPulseStruct[P]
}

/**
  * Wrapper for a struct type combining GraphSpore and PulsingSpore
  */
trait ChangableGraphStruct extends Struct {
  override type StructType[P, R] <: GraphStruct[R] with ReadWritePulseStruct[P]
}

trait ReadPulseStruct[P] {
  def base(implicit turn: Turn[_]): Pulse[P]
  def get(implicit turn: Turn[_]): Pulse[P]
}

/**
  * Spore that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update
  *
  * @tparam P Pulse stored value type
  */
trait ReadWritePulseStruct[P] <: ReadPulseStruct[P] {
  def set(value: Pulse[P])(implicit turn: Turn[_]): Unit

}

trait ReadGraphStruct[R] {
  def incoming(implicit turn: Turn[_]): Set[R]
}

/**
  * Spore that can represent a node in a graph by providing information about incoming and outgoing edges.
  *
  * @tparam R Type of the reactive values that are connected to this struct
  */
trait GraphStruct[R] extends ReadGraphStruct[R] {
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit
  def outgoing(implicit turn: Turn[_]): Iterator[R]
  def discover(reactive: R)(implicit turn: Turn[_]): Unit
  def drop(reactive: R)(implicit turn: Turn[_]): Unit
}
