package rescala.core

import rescala.macros.MacroAccessors
import rescala.reactives.{Event, Signal}

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.language.implicitConversions

class Ticket[S <: Struct](val creation: Initializer[S])

abstract class ReevTicket[T, S <: Struct](creation: Initializer[S]) extends DynamicTicket[S](creation) with Result[T, S] {

  // schedulers implement these to allow access
  protected def staticAfter[A](reactive: ReSourciV[A, S]): A
  protected def dynamicAfter[A](reactive: ReSourciV[A, S]): A

  // dependency tracking accesses
  private[rescala] override def dependStatic[A](reactive: ReSourciV[A, S]): A = {
    if (collectedDependencies != null) collectedDependencies += reactive
    staticAfter(reactive)
  }

  private[rescala] override def dependDynamic[A](reactive: ReSourciV[A, S]): A = {
    if (collectedDependencies != null) collectedDependencies += reactive
    dynamicAfter(reactive)
  }

  // inline result into ticket, to reduce the amount of garbage during reevaluation
  private var collectedDependencies: Set[ReSource[S]] = null
  private var propagate = false
  private var value: T = _
  private var effect: () => Unit = null
  final def trackDependencies(): Unit = collectedDependencies = Set.empty
  final def withPropagate(p: Boolean): ReevTicket[T, S] = {propagate = p; this}
  final def withValue(v: T): ReevTicket[T, S] = {require(v != null, "value must not be null"); value = v; propagate = true; this}
  final def withEffect(v: () => Unit): ReevTicket[T, S] = {effect = v; this}

  final override def forValue(f: T => Unit): Unit = if (value != null) f(value)
  final override def forEffect(f: (() => Unit) => Unit): Unit = if (effect != null) f(effect)
  final override def getDependencies(): Option[Set[ReSource[S]]] = Option(collectedDependencies)

  final def reset[NT](): ReevTicket[NT, S] = {
    propagate = false
    value = null.asInstanceOf[T]
    effect = null
    collectedDependencies = null
    this.asInstanceOf[ReevTicket[NT, S]]
  }

}

abstract class DynamicTicket[S <: Struct](creation: Initializer[S]) extends StaticTicket[S](creation) {
  final def read[V, A](reactive: MacroAccessors[V, A, S]): A = reactive.interpret(dependDynamic(reactive))
  private[rescala] def dependDynamic[A](reactive: ReSourciV[A, S]): A
}

sealed abstract class StaticTicket[S <: Struct](creation: Initializer[S]) extends Ticket(creation) {
  final def staticRead[V, A](reactive: MacroAccessors[V, A, S]): A = reactive.interpret(dependStatic(reactive))
  private[rescala] def dependStatic[A](reactive: ReSourciV[A, S]): A
}


abstract class InitialChange[S <: Struct]{
  val source: ReSource[S]
  def accept(before: source.Value): Boolean
  def value: source.Value
}

abstract class AdmissionTicket[S <: Struct](creation: Initializer[S]) extends Ticket(creation) {

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
final case class CreationTicket[S <: Struct](self: Either[Initializer[S], Scheduler[S]])(val rename: REName) {

  def isInnerTicket(): Boolean = self.isLeft

  def apply[T](f: Initializer[S] => T): T = self match {
    case Left(integrated) => f(integrated)
    case Right(engine) => engine.create(f)
  }
}

object CreationTicket extends LowPriorityCreationImplicits {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: Ticket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))(line)

  implicit def fromCreationImplicit[S <: Struct](implicit creation: Initializer[S], line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
  implicit def fromCreation[S <: Struct](creation: Initializer[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
}

sealed trait LowPriorityCreationImplicits {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Scheduler[S], line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromEngine[S <: Struct](factory: Scheduler[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromNameImplicit[S <: Struct](line: String)(implicit outer: CreationTicket[S]): CreationTicket[S] = CreationTicket(outer.self)(line)
}
