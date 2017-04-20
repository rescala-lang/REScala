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
  type State[P, S <: Struct]

  type Ticket[S <: Struct] = ATicket[S]
}

trait ATicket[S <: Struct] {
  def dynamic(): DynamicTicket[S]
  def static(): StaticTicket[S]
  def turn(): Turn[S]
}





