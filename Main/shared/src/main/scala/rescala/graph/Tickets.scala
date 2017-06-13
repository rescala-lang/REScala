package rescala.graph


import rescala.engine.{Engine, Turn, ValuePersistency}
import rescala.reactives.{Event, Signal}

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

/* tickets are created by the REScala schedulers, to restrict operations to the correct scopes */

// thoughts regarding now, before and after:
// before: can be used at any time during turns. Thus its parameter should accept implicit turns only.
//         However, we want to be able to invoke .before inside, e.g., the closure of a .fold, where the turn
//         is not present in the scope. Thus before also accepts engines, but dynamically checks, that this engine
//         has a current turn set. This could probably be ensured statically by making sure that every reactive
//         definition site somehow provides the reevaluating ticket as an implicit in the scope, but I'm not sure
//         if this is possible without significant syntactical inconvenience.
//  after: falls under the same considerations as before, with the added exception that it should only accept
//         turns that completed their admission phase and started their propagation phase. This is currently not
//         checked at all, but could also be ensured statically the same as above.
//    now: can be used inside turns only during admission and wrapup, or outside of turns at all times. Since during
//         admission and wrapup, a corresponding OutsidePropagationTicket is in scope, it accepts these tickets as
//         high priority implicit parameters. In case such a ticket is not available, it accepts engines, and then
//         dynamically checks that the engine does NOT have a current turn (in the current thread context). I think
//         this cannot be ensured statically, as users can always hide implicitly available current turns.

trait AlwaysTicket[S <: Struct] extends Any {
  //TODO: only used to create TurnSource … maybe we can improve TurnSource, now that it is mostly used for creation?
  private[rescala] def turn: Turn[S]
}

sealed trait OutsidePropagationTicket[S <: Struct] extends Any with AlwaysTicket[S]

sealed trait PropagationAndLaterTicket[S <: Struct] extends Any with AlwaysTicket[S]

final class DynamicTicket[S <: Struct] private[rescala] (val turn: Turn[S], val indepsBefore: Set[Reactive[S]]) extends PropagationAndLaterTicket[S] {
  private[rescala] var indepsAfter: Set[Reactive[S]] = Set.empty
  private[rescala] var indepsAdded: Set[Reactive[S]] = Set.empty

  private[rescala] def dynamicDepend[A](reactive: Pulsing[A, S]): A = {
    if(indepsBefore(reactive)) {
      indepsAfter += reactive
      turn.staticAfter(reactive)
    } else if(indepsAdded(reactive)) {
      turn.staticAfter(reactive)
    } else {
      indepsAfter += reactive
      indepsAdded += reactive
      turn.dynamicAfter(reactive)
    }
  }

  def before[A](reactive: Signal[A, S]): A = {
    turn.dynamicBefore(reactive).get
  }

  def depend[A](reactive: Signal[A, S]): A = {
    dynamicDepend(reactive).get
  }

  def depend[A](reactive: Event[A, S]): Option[A] = {
    dynamicDepend(reactive).toOption
  }

  def indepsRemoved = indepsBefore.diff(indepsAfter)
}

final class StaticTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] {
  private[rescala] def staticBefore[A](reactive: Pulsing[A, S]): A = {
    turn.staticBefore(reactive)
  }
  private[rescala] def staticDepend[A](reactive: Pulsing[A, S]): A = {
    turn.staticAfter(reactive)
  }
}

final class AdmissionTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with OutsidePropagationTicket[S] {
  def now[A](reactive: Signal[A, S]): A = {
    turn.dynamicBefore(reactive).get
  }
}

final class WrapUpTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] with OutsidePropagationTicket[S] {
  def now[A](reactive: Signal[A, S]): A = {
    turn.dynamicAfter(reactive).get
  }
  def now[A](reactive: Event[A, S]): Option[A] = {
    turn.dynamicAfter(reactive).toOption
  }
}

final class InnerCreationTicket[S <: Struct](val turn: Turn[S]) extends AnyVal with AlwaysTicket[S]  {
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
    turn.create(incoming, valuePersistency)(instantiateReactive)
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
final case class CreationTicket[S <: Struct](self: Either[Turn[S], Engine[S]]) extends AnyVal {

  def apply[T](f: InnerCreationTicket[S] => T): T = self match {
    case Left(turn) => f(new InnerCreationTicket[S](turn))
    case Right(engine) => engine.currentTurn() match {
      case Some(turn) => f(new InnerCreationTicket[S](turn))
      case None => engine.executeTurn(Set.empty, ticket => f(new InnerCreationTicket(ticket.turn)), engine.noWrapUp[T])
    }
  }
}

object CreationTicket extends LowPriorityTurnSource {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: AlwaysTicket[S]): CreationTicket[S] = CreationTicket(Left(ticket.turn))
  implicit def fromTicket[S <: Struct](ticket: AlwaysTicket[S]): CreationTicket[S] = CreationTicket(Left(ticket.turn))
}

sealed trait LowPriorityTurnSource {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Engine[S]): CreationTicket[S] = CreationTicket(Right(factory))
  implicit def fromEngine[S <: Struct](factory: Engine[S]): CreationTicket[S] = CreationTicket(Right(factory))
}
