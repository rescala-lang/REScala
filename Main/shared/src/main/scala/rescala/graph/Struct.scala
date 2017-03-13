package rescala.graph

import rescala.propagation.{DynamicTicket, StaticTicket, Turn}

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
  type Type[P, S <: Struct] <: ReadValue[P, S]

  type Ticket[S <: Struct] <: ATicket[S]
}

trait ATicket[S <: Struct] {
  def dynamic(): DynamicTicket[S]
  def static(): StaticTicket[S]
  def turn(): Turn[S]
}

/**
  * Wrapper for a struct type combining GraphSpore and PulsingSpore
  */
trait GraphStruct extends Struct {
  override type Type[P, S <: Struct] <: GraphStructType[S] with ReadWriteValue[P, S]
  override type Ticket[S <: Struct] = ATicket[S]

}

trait ReadValue[P, S <: Struct] {
  def base(implicit turn: S#Ticket[S]): P
  def get(implicit turn: S#Ticket[S]): P
}

/**
  * Spore that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update
  *
  * @tparam P Pulse stored value type
  */
trait ReadWriteValue[P, S <: Struct] <: ReadValue[P, S] {
  def set(value: P)(implicit turn: S#Ticket[S]): Unit
}

/**
  * Spore that can represent a node in a graph by providing information about incoming and outgoing edges.
  *
  * @tparam R Type of the reactive values that are connected to this struct
  */
trait GraphStructType[S <: Struct] {
  def incoming(implicit turn: Turn[S]): Set[Reactive[S]]
  def updateIncoming(reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit
  def outgoing(implicit turn: Turn[S]): Iterator[Reactive[S]]
  def discover(reactive: Reactive[S])(implicit turn: Turn[S]): Unit
  def drop(reactive: Reactive[S])(implicit turn: Turn[S]): Unit
}
