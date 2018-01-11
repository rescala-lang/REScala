package rescala.core

import rescala.reactives.{Event, Signal}

import scala.annotation.implicitNotFound
import scala.collection.mutable
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


trait AnyTicket extends Any

final class DynamicTicket[S <: Struct] private[rescala](val creation: ComputationStateAccess[S] with Creation[S], val indepsBefore: Set[ReSource[S]]) extends AnyTicket {
  private[rescala] var indepsAfter: Set[ReSource[S]] = Set.empty
  private[rescala] var indepsAdded: Set[ReSource[S]] = Set.empty

  private[rescala] def dependDynamic[A](reactive: ReSourciV[A, S]): A = {
    if (indepsBefore(reactive)) {
      indepsAfter += reactive
      creation.staticAfter(reactive)
    }
    else if (indepsAdded(reactive)) {
      creation.staticAfter(reactive)
    }
    else {
      indepsAfter += reactive
      indepsAdded += reactive
      creation.dynamicAfter(reactive)
    }
  }

  private[rescala] def dependStatic[A](reactive: ReSourciV[A, S]): A = {
    // static dependencies are currently not optimized, for mixed usages
    dependDynamic(reactive)
  }

  def before[A](reactive: Signal[A, S]): A = {
    creation.dynamicBefore(reactive).get
  }

  def depend[A](reactive: Signal[A, S]): A = {
    dependDynamic(reactive).get
  }

  def depend[A](reactive: Event[A, S]): Option[A] = {
    dependDynamic(reactive).toOption
  }

  def staticDepend[A](reactive: Signal[A, S]): A = {
    dependStatic(reactive).get
  }

  def staticDepend[A](reactive: Event[A, S]): Option[A] = {
    dependStatic(reactive).toOption
  }

  def indepsRemoved: Set[ReSource[S]] = indepsBefore.diff(indepsAfter)
}

final class StaticTicket[S <: Struct] private[rescala](val creation: ComputationStateAccess[S] with Creation[S]) extends AnyVal with AnyTicket {
  private[rescala]  def staticBefore[A](reactive: ReSourciV[A, S]): A = {
    creation.staticBefore(reactive)
  }
  private[rescala] def staticDependPulse[A](reactive: ReSourciV[A, S]): A = {
    creation.staticAfter(reactive)
  }

  def staticDepend[A](reactive: Signal[A, S]): A = {
    staticDependPulse(reactive: ReSourciV[Pulse[A], S]).get
  }

  def staticDepend[A](reactive: Event[A, S]): Option[A] = {
    staticDependPulse(reactive: ReSourciV[Pulse[A], S]).toOption
  }
}


abstract class InitialChange[S <: Struct]{
  val source: ReSource[S]
  def value: source.Value
}

final class AdmissionTicket[S <: Struct] private[rescala](val creation: ComputationStateAccess[S] with Creation[S]) extends AnyTicket {

  private[rescala] var wrapUp: WrapUpTicket[S] => Unit = null

  private val _initialChanges: mutable.Map[ReSource[S], InitialChange[S]] = mutable.HashMap()
  private[rescala] def initialChanges: collection.Map[ReSource[S], InitialChange[S]] = _initialChanges
  private[rescala] def recordChange[T](ic: InitialChange[S]): Unit = {
    assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
    _initialChanges.put(ic.source, ic)
  }
  def now[A](reactive: Signal[A, S]): A = {
    creation.dynamicBefore(reactive).get
  }
}

final class WrapUpTicket[S <: Struct] private[rescala](val creation: ComputationStateAccess[S] with Creation[S]) extends AnyVal with AnyTicket {
  def now[A](reactive: Signal[A, S]): A = {
    creation.dynamicAfter(reactive).get
  }
  def now[A](reactive: Event[A, S]): Option[A] = {
    creation.dynamicAfter(reactive).toOption
  }
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
final case class CreationTicket[S <: Struct](self: Either[Creation[S], Engine[S]])(val rename: REName) {

  def isInnerTicket(): Boolean = self.isLeft

  def apply[T](f: Creation[S] => T): T = self match {
    case Left(integrated) => f(integrated)
    case Right(engine) => engine.create(f)
  }
}

object CreationTicket extends LowPriorityCreationImplicits {
  implicit def fromTicketAImplicit[S <: Struct](implicit ticket: AdmissionTicket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))(line)
  //implicit def fromTicketA[S <: Struct](ticket: AdmissionTicket[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))

  implicit def fromTicketSImplicit[S <: Struct](implicit ticket: StaticTicket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))(line)
  //implicit def fromTicketS[S <: Struct](ticket: StaticTicket[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))

  implicit def fromTicketDImplicit[S <: Struct](implicit ticket: DynamicTicket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))(line)
  //implicit def fromTicketD[S <: Struct](ticket: DynamicTicket[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))

  implicit def fromCreationImplicit[S <: Struct](implicit creation: Creation[S], line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
  implicit def fromCreation[S <: Struct](creation: Creation[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
}

sealed trait LowPriorityCreationImplicits {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Engine[S], line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromEngine[S <: Struct](factory: Engine[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromNameImplicit[S <: Struct](line: String)(implicit factory: Engine[S]): CreationTicket[S] = CreationTicket(Right(factory))(line)
}
