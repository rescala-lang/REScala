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


class Ticket[S <: Struct](val creation: Creation[S])

abstract class ReevTicket[T, S <: Struct](creation: Creation[S]) extends DynamicTicket[S](creation) with Result[T, S] {
  def trackDependencies(): Unit = collectedDependencies = Set.empty
  private var collectedDependencies: Set[ReSource[S]] = null
  def getDependencies(): Option[Set[ReSource[S]]] = Option(collectedDependencies)

  override def readStatic[A](reactive: ReSourciV[A, S]): A = {
    if (collectedDependencies != null) collectedDependencies += reactive
    staticAfter(reactive)
  }

  def readDynamic[A](reactive: ReSourciV[A, S]): A = {
    if (collectedDependencies != null) collectedDependencies += reactive
    dynamicAfter(reactive)
  }

  protected def staticAfter[A](reactive: ReSourciV[A, S]): A
  protected def dynamicAfter[A](reactive: ReSourciV[A, S]): A


  // inline result into ticket, to reduce the amount of garbage during reevaluation
  var propagate = false
  var value: T = _
  var effect: () => Unit = null
  final def withPropagate(p: Boolean): ReevTicket[T, S] = {propagate = p; this}
  final def withValue(v: T): ReevTicket[T, S] = {value = v; propagate = true; this}
  final def withEffect(v: () => Unit): ReevTicket[T, S] = {effect = v; this}

  override def forValue(f: T => Unit): Unit = if (value != null) f(value)
  override def forEffect(f: (() => Unit) => Unit): Unit = if (effect != null) f(effect)

  def reset[NT](): ReevTicket[NT, S] = {
    propagate = false
    value = null.asInstanceOf[T]
    effect = null
    collectedDependencies = null
    this.asInstanceOf[ReevTicket[NT, S]]
  }

}

abstract class DynamicTicket[S <: Struct](creation: Creation[S]) extends StaticTicket[S](creation) {

  def depend[A](reactive: Signal[A, S]): A = readDynamic(reactive).get
  def depend[A](reactive: Event[A, S]): Option[A] = readDynamic(reactive).toOption
  def readDynamic[A](reactive: ReSourciV[A, S]): A
}

sealed abstract class StaticTicket[S <: Struct](creation: Creation[S]) extends Ticket(creation) {
  def staticDepend[A](reactive: Signal[A, S]): A = readStatic(reactive).get
  def staticDepend[A](reactive: Event[A, S]): Option[A] = readStatic(reactive).toOption
  def readStatic[A](reactive: ReSourciV[A, S]): A
}


abstract class InitialChange[S <: Struct]{
  val source: ReSource[S]
  def accept(before: source.Value): Boolean
  def value: source.Value
}

abstract class AdmissionTicket[S <: Struct](creation: Creation[S]) extends Ticket(creation) {

  private[rescala] var wrapUp: WrapUpTicket[S] => Unit = null

  private val _initialChanges: mutable.Map[ReSource[S], InitialChange[S]] = mutable.HashMap()
  private[rescala] def initialChanges: collection.Map[ReSource[S], InitialChange[S]] = _initialChanges
  private[rescala] def recordChange[T](ic: InitialChange[S]): Unit = {
    assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
    _initialChanges.put(ic.source, ic)
  }
  final def now[A](reactive: Signal[A, S]): A = read(reactive).get

  def read[A](reactive: ReSourciV[A, S]): A

}


abstract class WrapUpTicket[S <: Struct] {
  def dynamicAfter[A](reactive: ReSourciV[A, S]): A
  final def now[A](reactive: Signal[A, S]): A = dynamicAfter(reactive).get
  final def now[A](reactive: Event[A, S]): Option[A] = dynamicAfter(reactive).toOption
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
final case class CreationTicket[S <: Struct](self: Either[Creation[S], Scheduler[S]])(val rename: REName) {

  def isInnerTicket(): Boolean = self.isLeft

  def apply[T](f: Creation[S] => T): T = self match {
    case Left(integrated) => f(integrated)
    case Right(engine) => engine.create(f)
  }
}

object CreationTicket extends LowPriorityCreationImplicits {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: Ticket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))(line)

  implicit def fromCreationImplicit[S <: Struct](implicit creation: Creation[S], line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
  implicit def fromCreation[S <: Struct](creation: Creation[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
}

sealed trait LowPriorityCreationImplicits {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Scheduler[S], line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromEngine[S <: Struct](factory: Scheduler[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromNameImplicit[S <: Struct](line: String)(implicit outer: CreationTicket[S]): CreationTicket[S] = CreationTicket(outer.self)(line)
}
