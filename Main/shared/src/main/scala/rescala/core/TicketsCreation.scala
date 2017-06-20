package rescala.core

import scala.annotation.implicitNotFound
import scala.language.implicitConversions


final class InnerCreationTicket[S <: Struct](val creation: Creation[S]) extends AnyVal with CreationIntegrated[S]  {
  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param incoming a set of incoming dependencies
    * @param valuePersistency the value persistency
    * @param instantiateReactive The factory method to instantiate the reactive with the newly created state.
    * @tparam P Reactive value type
    * @tparam R Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  private[rescala] def create[P, R <: Reactive[S]](incoming: Set[Reactive[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => R): R =
    creation.create(incoming, valuePersistency)(instantiateReactive)
}


/**
  * A turn source that stores a turn/engine and can be applied to a function to call it with the appropriate turn as parameter.
  *
  * @param self Turn or engine stored by the turn source
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
@implicitNotFound(msg = "could not generate a turn source." +
  " An available implicit Ticket will serve as turn source, or if no" +
  " such turn is present, an implicit Engine is accepted instead.")
final case class CreationTicket[S <: Struct](self: Either[CreationIntegrated[S], Engine[S]]) extends AnyVal {

  def apply[T](f: InnerCreationTicket[S] => T): T = self match {
    case Left(integrated) => f(new InnerCreationTicket[S](integrated.creation))
    case Right(engine) => engine.create(f)
  }
}

object CreationTicket extends LowPriorityCreationImplicits {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: CreationIntegrated[S]): CreationTicket[S] = CreationTicket(Left(ticket))
  implicit def fromTicket[S <: Struct](ticket: CreationIntegrated[S]): CreationTicket[S] = CreationTicket(Left(ticket))
}

sealed trait LowPriorityCreationImplicits {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Engine[S]): CreationTicket[S] = CreationTicket(Right(factory))
  implicit def fromEngine[S <: Struct](factory: Engine[S]): CreationTicket[S] = CreationTicket(Right(factory))
}
