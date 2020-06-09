package rescala.core

import rescala.core.Initializer.InitValues
import rescala.reactives.RExceptions

import scala.annotation.implicitNotFound

/** [[InnerTicket]]s are used in Rescala to give capabilities to contexts during propagation.
  * [[ReevTicket]] is used during reevaluation, and [[AdmissionTicket]] during the initialization. */
class InnerTicket[S <: Struct](val initializer: Initializer[S])

/** [[ReevTicket]] is given to the [[Derived]] reevaluate method and allows to access other reactives.
  * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
  * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
  * */
abstract class ReevTicket[V, S <: Struct](initializer: Initializer[S],
                                          private var _before: V
                                         ) extends DynamicTicket[S](initializer) with Result[V, S] {

  // schedulers implement these to allow access
  protected def staticAccess(reactive: ReSource[S]): reactive.Value
  protected def dynamicAccess(reactive: ReSource[S]): reactive.Value

  // dependency tracking accesses
  private[rescala] final override def collectStatic(reactive: ReSource[S]): reactive.Value = {
    assert(collectedDependencies == null || collectedDependencies.contains(reactive))
    staticAccess(reactive)
  }

  private[rescala] final override def collectDynamic(reactive: ReSource[S]): reactive.Value = {
    assert (collectedDependencies != null, "may not access dynamic dependencies without tracking dependencies")
    val updatedDeps = collectedDependencies + reactive
    if(updatedDeps eq collectedDependencies) {
      staticAccess(reactive)
    } else {
      collectedDependencies = updatedDeps
      dynamicAccess(reactive)
    }
  }

  // inline result into ticket, to reduce the amount of garbage during reevaluation
  private var collectedDependencies: Set[ReSource[S]] = null

  private var _propagate = false
  private var value: V = _
  private var effect: Observation = null
  override final def toString: String = s"Result(value = $value, propagate = $propagate, deps = $collectedDependencies)"
  final def before: V = _before
  /** Advises the ticket to track dynamic dependencies.
    * The passed initial set of dependencies may be processed as if they were static,
    * and are also returned in the resulting dependencies. */
  final def trackDependencies(initial: Set[ReSource[S]]): ReevTicket[V, S] = {collectedDependencies = initial; this}
  final def trackStatic(): ReevTicket[V, S] = {collectedDependencies = null; this}
  final def withPropagate(p: Boolean): ReevTicket[V, S] = {_propagate = p; this}
  final def withValue(v: V): ReevTicket[V, S] = {require(v != null, "value must not be null"); value = v; _propagate = true; this}
  final def withEffect(v: Observation): ReevTicket[V, S] = {effect = v; this}


  final override def propagate: Boolean = _propagate
  final override def forValue(f: V => Unit): Unit = if (value != null) f(value)
  final override def forEffect(f: Observation => Unit): Unit = if (effect != null) f(effect)
  final override def getDependencies(): Option[Set[ReSource[S]]] = Option(collectedDependencies)

  final def reset[NT](nb: NT): ReevTicket[NT, S] = {
    _propagate = false
    value = null.asInstanceOf[V]
    effect = null
    collectedDependencies = null
    val res = this.asInstanceOf[ReevTicket[NT, S]]
    res._before = nb
    res
  }

}

/** User facing low level API to access values in a dynamic context. */
abstract class DynamicTicket[S <: Struct](creation: Initializer[S]) extends StaticTicket[S](creation) {
  private[rescala] def collectDynamic(reactive: ReSource[S]): reactive.Value
  final def depend[A](reactive: Interp[A, S]): A = reactive.interpret(collectDynamic(reactive))
}

/** User facing low level API to access values in a static context. */
sealed abstract class StaticTicket[S <: Struct](creation: Initializer[S]) extends InnerTicket(creation) {
  private[rescala] def collectStatic(reactive: ReSource[S]): reactive.Value
  final def dependStatic[A](reactive: Interp[A, S]): A = reactive.interpret(collectStatic(reactive))
}

trait InitialChange[S <: Struct] {
  val source: ReSource[S]
  def writeValue(b: source.Value, v: source.Value => Unit): Boolean
}

/** Enables reading of the current value during admission.
  * Keeps track of written sources internally. */
abstract class AdmissionTicket[S <: Struct](initializer: Initializer[S], declaredWrites: Set[ReSource[S]])
  extends InnerTicket(initializer) with AccessTicket[S] {

  private var _initialChanges = Map[ReSource[S], InitialChange[S]]()
  private[rescala] def initialChanges: Map[ReSource[S], InitialChange[S]] = _initialChanges
  private[rescala] def recordChange[T](ic: InitialChange[S]): Unit = {
    assert(declaredWrites.contains(ic.source), "must not set a source that has not been pre-declared for the transaction")
    assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
    _initialChanges += ic.source -> ic
  }

  private[rescala] var wrapUp: AccessTicket[S] => Unit = null
}


trait AccessTicket[S <: Struct] {
  private[rescala] def access(reactive: ReSource[S]): reactive.Value
  final def now[A](reactive: Interp[A, S]): A = {
    RExceptions.toExternalReadException(reactive, reactive.interpret(access(reactive)))
  }
}


/** Enables the creation of other reactives */
@implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
final case class CreationTicket[S <: Struct](self: Either[Initializer[S], Scheduler[S]], rename: REName) {

  private[rescala] def create[V, T <: Derived[S]](incoming: Set[ReSource[S]],
                                                  initv   : InitValues[V],
                                                  inite   : Boolean)
                                                 (instantiateReactive: S#State[V, S] => T): T = {
    transaction(_.create(incoming, initv, inite, this)(instantiateReactive))
  }
  private[rescala] def createSource[V, T <: ReSource[S]]
    (intv: InitValues[V])(instantiateReactive: S#State[V, S] => T): T = {

    transaction(_.createSource(intv, this)(instantiateReactive))
  }

  /** Returns true if this ticket is already part of a transaction. */
  def isInnerTicket(): Boolean = self.isLeft
  /** Using the ticket requires to create a new scope, such that we can ensure that everything happens in the same transaction */
  def transaction[T](f: Initializer[S] => T): T = self match {
    case Left(integrated) => f(integrated)
    case Right(engine) => engine.initializerDynamicLookup(f)
  }
}

/** As reactives can be created during propagation, any [[InnerTicket]] can be converted to a creation ticket. */
object CreationTicket extends LowPriorityCreationImplicits {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: InnerTicket[S], line: REName): CreationTicket[S] = CreationTicket(Left(ticket.initializer), line)

  implicit def fromInitializerImplicit[S <: Struct](implicit initializer: Initializer[S], line: REName): CreationTicket[S] = CreationTicket(Left(initializer), line)
  implicit def fromInitializer[S <: Struct](creation: Initializer[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Left(creation), line)
}

/** If no [[InnerTicket]] is found, then these implicits will search for a [[Scheduler]],
  * creating the reactives outside of any turn. */
sealed trait LowPriorityCreationImplicits {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Scheduler[S], line: REName): CreationTicket[S] = CreationTicket(Right(factory), line)
  implicit def fromEngine[S <: Struct](factory: Scheduler[S])(implicit line: REName): CreationTicket[S] = CreationTicket(Right(factory), line)
  implicit def fromNameImplicit[S <: Struct](line: String)(implicit outer: CreationTicket[S]): CreationTicket[S] = CreationTicket(outer.self, line)
}
