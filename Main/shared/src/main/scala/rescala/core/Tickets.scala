package rescala.core

import rescala.macros.Interp
import rescala.reactives.Signal

import scala.annotation.implicitNotFound
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

/** [[InnerTicket]]s are used in Rescala to give capabilities to contexts during propagation.
  * [[ReevTicket]] is used during reevaluation, and [[AdmissionTicket]] during the initialization. */
class InnerTicket[S <: Struct](val creation: Initializer[S])

/** [[ReevTicket]] is given to the [[Reactive]] reevaluate method and allows to access other reactives.
  * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
  * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
  * */
abstract class ReevTicket[V, N, S <: Struct](
                                              creation: Initializer[S],
                                              private var _before: V,
                                            ) extends DynamicTicket[S](creation) with Result[V, N, S] {

  // schedulers implement these to allow access
  protected def staticAccess[A](reactive: ReSource[S]): (reactive.Value, reactive.Notification)
  protected def dynamicAccess[A](reactive: ReSource[S]): (reactive.Value, reactive.Notification)

  // dependency tracking accesses
  private[rescala] final override def collectStatic[A](reactive: ReSource[S]): (reactive.Value, reactive.Notification) = {
    if (collectedDependencies != null) collectedDependencies += reactive
    staticAccess(reactive)
  }

  private[rescala] final override def collectDynamic[A](reactive: ReSource[S]): (reactive.Value, reactive.Notification) = {
    if (collectedDependencies != null) collectedDependencies += reactive
    dynamicAccess(reactive)
  }

  // inline result into ticket, to reduce the amount of garbage during reevaluation
  private var collectedDependencies: Set[ReSource[S]] = null

  private var _propagate = false
  private var value: V = _
  private var notification: N = _
  private var effect: () => Unit = null
  final def before: V = _before
  final def trackDependencies(): Unit = collectedDependencies = Set.empty
  final def withPropagate(p: Boolean): ReevTicket[V, N, S] = {_propagate = p; this}
  final def withNotification(n: N): ReevTicket[V, N, S] = {require(n != null, "notification must not be null"); notification = n; _propagate = true; this}
  final def withValue(v: V): ReevTicket[V, N, S] = {require(v != null, "value must not be null"); value = v; _propagate = true; this}
  final def withEffect(v: () => Unit): ReevTicket[V, N, S] = {effect = v; this}


  final override def propagate: Boolean = _propagate
  final override def forValue(f: V => Unit): Unit = if (value != null) f(value)
  final override def forEffect(f: (() => Unit) => Unit): Unit = if (effect != null) f(effect)
  final override def forNotification(f: N => Unit): Unit = if (notification != null) f(notification)
  final override def getDependencies(): Option[Set[ReSource[S]]] = Option(collectedDependencies)

  final def reset[NT, NN](nb: NT): ReevTicket[NT, NN, S] = {
    _propagate = false
    value = null.asInstanceOf[V]
    effect = null
    collectedDependencies = null
    val res = this.asInstanceOf[ReevTicket[NT, NN, S]]
    res._before = nb
    res
  }

}

/** User facing low level API to access values in a dynamic context. */
abstract class DynamicTicket[S <: Struct](creation: Initializer[S]) extends StaticTicket[S](creation) {
  private[rescala] def collectDynamic[A](reactive: ReSource[S]): (reactive.Value, reactive.Notification)
  final def depend[A](reactive: Interp[S, A]): A = (reactive.interpret _).tupled(collectDynamic(reactive))
}

/** User facing low level API to access values in a static context. */
sealed abstract class StaticTicket[S <: Struct](creation: Initializer[S]) extends InnerTicket(creation) {
  private[rescala] def collectStatic[A](reactive: ReSource[S]): (reactive.Value, reactive.Notification)
  final def dependStatic[A](reactive: Interp[S, A]): A = (reactive.interpret _).tupled(collectStatic(reactive))

}

sealed trait InitialChange[S <: Struct] {
  def source: ReSource[S]
}
/** Records the initial source changes to be propagated */
abstract class InitialChangeV[S <: Struct] extends InitialChange[S] {
  val source: ReSource[S]
  def value: source.Value
  /** Returns true iff the new [[value]] should be propagated, given the old value. */
  def accept(before: source.Value): Boolean
}

/** Records the initial source changes to be propagated */
abstract class InitialChangeN[S <: Struct] extends InitialChange[S] {
  val source: ReSource[S]
  def value: source.Notification
}

/** Enables reading of the current value during admission.
  * Keeps track of written sources internally. */
abstract class AdmissionTicket[S <: Struct](creation: Initializer[S]) extends InnerTicket(creation) {
  def access[A](reactive: Signal[A, S]): A
  final def now[A](reactive: Signal[A, S]): A = access(reactive)

  private val _initialChanges = ArrayBuffer[InitialChange[S]]()
  private[rescala] def initialChanges: Iterable[InitialChange[S]] = _initialChanges
  private[rescala] def recordChange[T](ic: InitialChange[S]): Unit = {
    assert(!_initialChanges.exists(c => c.source == ic.source), "must not admit same source twice in one turn")
    _initialChanges += ic
  }

  private[rescala] var wrapUp: WrapUpTicket[S] => Unit = null
}


abstract class WrapUpTicket[S <: Struct] {
  private[rescala] def access[A](reactive: ReSource[S]): reactive.Notification
  final def now[A](reactive: Interp[S, A]): A = ???
}


/** Enables the creation of other reactives */
@implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
final case class CreationTicket[S <: Struct](self: Either[Initializer[S], Scheduler[S]])(val rename: REName) {

  def isInnerTicket(): Boolean = self.isLeft
  /** Using the ticket requires to create a new scope, such that we can ensure that everything happens in the same transaction */
  def apply[T](f: Initializer[S] => T): T = self match {
    case Left(integrated) => f(integrated)
    case Right(engine) => engine.create(f)
  }
}

/** As reactives can be created during propagation, any [[InnerTicket]] can be converted to a creation ticket. */
object CreationTicket extends LowPriorityCreationImplicits {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: InnerTicket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.creation))(line)

  implicit def fromCreationImplicit[S <: Struct](implicit creation: Initializer[S], line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
  implicit def fromCreation[S <: Struct](creation: Initializer[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(creation))(line)
}

/** If no [[InnerTicket]] is found, then these implicits will search for a [[Scheduler]],
  * creating the reactives outside of any turn. */
sealed trait LowPriorityCreationImplicits {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Scheduler[S], line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromEngine[S <: Struct](factory: Scheduler[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Right(factory))(line)
  implicit def fromNameImplicit[S <: Struct](line: String)(implicit outer: CreationTicket[S]): CreationTicket[S] = CreationTicket(outer.self)(line)
}
