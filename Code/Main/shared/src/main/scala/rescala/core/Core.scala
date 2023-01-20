package rescala.core

import rescala.operator.RExceptions

import scala.annotation.{implicitNotFound, nowarn}
import scala.util.DynamicVariable

/** Source of (reactive) values. */
trait ReSource {
  type Value
  type State[_]
  protected[rescala] def state: State[Value]
  def name: ReName
  protected[rescala] def commit(base: Value): Value
}
object ReSource { type of[S[_]] = ReSource { type State[V] = S[V] } }

/** A reactive value is something that can be reevaluated */
trait Derived extends ReSource {

  final type ReIn = ReevTicket[State, Value]
  final type Rout = Result.of[State, Value]

  /** called if any of the dependencies ([[rescala.core.Core.ReSource]]s) changed in the current update turn,
    * after all (known) dependencies are updated
    */
  protected[rescala] def reevaluate(input: ReIn): Rout
}
object Derived { type of[S[_]] = Derived { type State[V] = S[V] } }

/** Base implementation for reactives, with [[Derived]] for scheduling,
  * together with a [[ReName]] and containing a [[State]]
  *
  * @param state the state passed by the scheduler
  * @param name  the name of the reactive, useful for debugging as it often contains positional information
  */
abstract class Base[S[_], V](override protected[rescala] val state: S[V], override val name: ReName)
    extends ReSource {
  override type State[V] = S[V]
  override type Value    = V
  override def toString: String = s"${name.str}($state)"
}

/** Common macro accessors for [[rescala.operator.SignalBundle.Signal]] and [[rescala.operator.EventBundle.Event]]
  *
  * @tparam A return type of the accessor
  * @groupname accessor Accessor and observers
  */
trait ReadAs[S[_], +A] extends ReSource {
  type State[V] = S[V]

  /** Interprets the internal type to the external type
    *
    * @group internal
    */
  def read(v: Value): A
}
object ReadAs { type of[S[_], A] = ReadAs[S, A] }

/** Encapsulates an action changing a single source. */
trait InitialChange[State[_]] {

  /** The source to be changed. */
  val source: ReSource.of[State]

  /** @param base         the current (old) value of the source.
    * @param writeCallback callback to apply the new value, executed only if the action is approved by the source.
    * @return the propagation status of the source (whether or not to reevaluate output reactives).
    */
  def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean
}

/** An initializer is the glue between that binds the creation of the reactive from the operator and scheduler side together.
  * The operator provides the logic to wrap a state and the scheduler provides the implementation of that state.
  * This is where the two are joined. After that, the new reactive may have to be initialized.
  */
trait Initializer {

  type State[_]

  /** Creates and correctly initializes new [[Derived]]s */
  final private[rescala] def create[V, T <: Derived.of[State]](
      incoming: Set[ReSource.of[State]],
      initValue: V,
      needsReevaluation: Boolean
  )(instantiateReactive: State[V] => T): T = {
    val state    = makeDerivedStructState[V](initValue)
    val reactive = instantiateReactive(state)
    register(reactive)
    initialize(reactive, incoming, needsReevaluation)
    reactive
  }

  /** hook for schedulers to globally collect all created resources, usually does nothing */
  protected[this] def register(reactive: ReSource.of[State]): Unit = ()

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[V, T <: ReSource.of[State]](
      intv: V
  )(instantiateReactive: State[V] => T): T = {
    val state    = makeSourceStructState[V](intv)
    val reactive = instantiateReactive(state)
    register(reactive)
    reactive
  }

  /** Creates the internal state of [[Derived]]s */
  protected[this] def makeDerivedStructState[V](initialValue: V): State[V]

  /** Creates the internal state of [[ReSource]]s */
  protected[this] def makeSourceStructState[V](initialValue: V): State[V] =
    makeDerivedStructState[V](initialValue)

  /** to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    *
    * @param reactive          the newly instantiated reactive
    * @param incoming          a set of incoming dependencies
    * @param needsReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def initialize(
      reactive: Derived.of[State],
      incoming: Set[ReSource.of[State]],
      needsReevaluation: Boolean
  ): Unit

}
object Initializer { type of[S[_]] = Initializer { type State[V] = S[V] } }

/** User facing low level API to access values in a static context. */
sealed abstract class StaticTicket[State[_]](val tx: Transaction.of[State]) {
  private[rescala] def collectStatic(reactive: ReSource.of[State]): reactive.Value
  final def dependStatic[A](reactive: ReadAs.of[State, A]): A = reactive.read(collectStatic(reactive))
}

/** User facing low level API to access values in a dynamic context. */
abstract class DynamicTicket[State[_]](tx: Transaction.of[State]) extends StaticTicket[State](tx) {
  private[rescala] def collectDynamic(reactive: ReSource.of[State]): reactive.Value
  final def depend[A](reactive: ReadAs.of[State, A]): A = reactive.read(collectDynamic(reactive))
}

trait AccessHandler[State[_]] {
  // schedulers implement these to allow access
  def staticAccess(reactive: ReSource.of[State]): reactive.Value
  def dynamicAccess(reactive: ReSource.of[State]): reactive.Value
}

/** [[ReevTicket]] is given to the [[Derived]] reevaluate method and allows to access other reactives.
  * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
  * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
  */
final class ReevTicket[S[_], V](tx: Transaction.of[S], private var _before: V, accessHandler: AccessHandler[S])
    extends DynamicTicket(tx)
    with Result[V] {

  override type State[V] = S[V]

  private var collectedDependencies: Set[ReSource.of[State]] = null

  // dependency tracking accesses
  private[rescala] override def collectStatic(reactive: ReSource.of[State]): reactive.Value = {
    assert(collectedDependencies == null || collectedDependencies.contains(reactive))
    accessHandler.staticAccess(reactive)
  }

  private[rescala] override def collectDynamic(reactive: ReSource.of[State]): reactive.Value = {
    assert(collectedDependencies != null, "may not access dynamic dependencies without tracking dependencies")
    val updatedDeps = collectedDependencies + reactive
    if (updatedDeps eq collectedDependencies) {
      accessHandler.staticAccess(reactive)
    } else {
      collectedDependencies = updatedDeps
      accessHandler.dynamicAccess(reactive)
    }
  }

  // inline result into ticket, to reduce the amount of garbage during reevaluation
  private var _propagate          = false
  private var value: V            = _
  private var effect: Observation = null
  override def toString: String =
    s"Result(value = $value, propagate = $activate, deps = $collectedDependencies)"
  def before: V = _before

  /** Advises the ticket to track dynamic dependencies.
    * The passed initial set of dependencies may be processed as if they were static,
    * and are also returned in the resulting dependencies.
    */
  def trackDependencies(initial: Set[ReSource.of[State]]): ReevTicket[State, V] = {
    collectedDependencies = initial; this
  }
  def trackStatic(): ReevTicket[State, V]             = { collectedDependencies = null; this }
  def withPropagate(p: Boolean): ReevTicket[State, V] = { _propagate = p; this }
  def withValue(v: V): ReevTicket[State, V] = {
    require(v != null, "value must not be null");
    value = v;
    _propagate = true;
    this
  }
  def withEffect(v: Observation): ReevTicket[State, V] = { effect = v; this }

  override def activate: Boolean                         = _propagate
  override def forValue(f: V => Unit): Unit              = if (value != null) f(value)
  override def forEffect(f: Observation => Unit): Unit   = if (effect != null) f(effect)
  override def inputs(): Option[Set[ReSource.of[State]]] = Option(collectedDependencies)

  def reset[NT](nb: NT): ReevTicket[State, NT] = {
    _propagate = false
    value = null.asInstanceOf[V]
    effect = null
    collectedDependencies = null
    val res = this.asInstanceOf[ReevTicket[State, NT]]
    res._before = nb
    res
  }
}

/** Result of a reevaluation */
trait Result[T] {

  type State[_]

  /** True iff outputs must also be reevaluated, false iff the propagation ends here. */
  def activate: Boolean

  /** No-allocation accessor for the optional new value. */
  def forValue(f: T => Unit): Unit

  /** No-allocation accessor for the effect caused by the reevaluation. */
  def forEffect(f: Observation => Unit): Unit

  /** New input resources.
    * None if unchanged.
    * Otherwise a list of all input reactives to react to.
    */
  def inputs(): Option[Set[ReSource.of[State]]]
}
object Result { type of[S[_], T] = Result[T] { type State[V] = S[V] } }

/** Records side effects for later execution. */
trait Observation { def execute(): Unit }

/** Enables reading of the current value during admission.
  * Keeps track of written sources internally.
  */
final class AdmissionTicket[State[_]](val tx: Transaction.of[State], declaredWrites: Set[ReSource.of[State]]) {

  private var _initialChanges: Map[ReSource.of[State], InitialChange[State]] =
    Map[ReSource.of[State], InitialChange[State]]()
  private[rescala] def initialChanges: Map[ReSource.of[State], InitialChange[State]] = _initialChanges
  def recordChange[T](ic: InitialChange[State]): Unit = {
    assert(
      declaredWrites.contains(ic.source),
      "must not set a source that has not been pre-declared for the transaction"
    )
    assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
    _initialChanges += ic.source -> ic
  }

  /** convenience method as many case studies depend on this being available directly on the AT */
  def now[A](reactive: ReadAs.of[State, A]): A = tx.now(reactive)

  private[rescala] var wrapUp: Transaction.of[State] => Unit = null
}

/** Enables the creation of other reactives */
@implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
final class CreationTicket[State[_]](val scope: ScopeSearch[State], val rename: ReName) {

  private[rescala] def create[V, T <: Derived.of[State]](
      incoming: Set[ReSource.of[State]],
      initValue: V,
      needsReevaluation: Boolean
  )(instantiateReactive: State[V] => T): T = {
    scope.embedTransaction { tx =>
      val init: Initializer.of[State] = tx.initializer
      init.create(incoming, initValue, needsReevaluation)(instantiateReactive)
    }
  }
  private[rescala] def createSource[V, T <: ReSource.of[State]](intv: V)(instantiateReactive: State[V] => T): T = {
    scope.embedTransaction(_.initializer.createSource(intv)(instantiateReactive))
  }
}

object CreationTicket {
  implicit def fromScope[State[_]](implicit scope: ScopeSearch[State], line: ReName): CreationTicket[State] =
    new CreationTicket(scope, line)
  // cases below are when one explicitly passes one of the parameters
  implicit def fromExplicitDynamicScope[S[_]](factory: DynamicScope[S])(implicit line: ReName): CreationTicket[S] =
    new CreationTicket[S](new ScopeSearch(Right(factory)) { type State[V] = S[V] }, line)
  implicit def fromTransaction[S[_]](tx: Transaction.of[S])(implicit line: ReName): CreationTicket[S] =
    new CreationTicket(new ScopeSearch[S](Left(tx)), line)
  implicit def fromName[State[_]](str: String)(implicit scopeSearch: ScopeSearch[State]): CreationTicket[State] =
    new CreationTicket(scopeSearch, ReName(str))

}

/** Essentially a kill switch, that will remove the reactive at some point. */
trait Disconnectable {
  def disconnect(): Unit
}

/** Removes the reactive instead of its next normal reevaluation.
  * This makes use of the fact, that all reactives are technically dynamic reactives,
  * and removing incoming dependencies is always kinda safe, as long as we are sure we no longer care!
  */
trait DisconnectableImpl extends Derived with Disconnectable {
  @volatile private var disconnected = false
  final def disconnect(): Unit = {
    disconnected = true
  }

  final override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    if (disconnected) {
      rein.trackDependencies(Set.empty)
      rein
    } else {
      guardedReevaluate(rein)
    }
  }

  protected[rescala] def guardedReevaluate(rein: ReIn): Rout

}

/** A transaction (or maybe transaction handle would be the better term) is available from reevaluation and admission tickets.
  * That is, everywhere during a transaction, you can read reactives, but also create them.
  * The reading values is core to any reactive propagation.
  * But creating reactives using the [[Initializer]] is a liability to the scheduler, but a superpower to the operators.
  * Its a classical tradeoff, but it would be better to not make this choice by default,
  * that is, reactive creation should be limited such that we can experiment with schedulers that do not have this liability.
  */
trait Transaction {

  type State[_]

  final def now[A](reactive: ReadAs.of[State, A]): A = {
    RExceptions.toExternalReadException(reactive, reactive.read(access(reactive)))
  }
  private[rescala] def access(reactive: ReSource.of[State]): reactive.Value

  def observe(obs: Observation): Unit

  def initializer: Initializer.of[State]
}
object Transaction {
  type of[S[_]] = Transaction { type State[A] = S[A] }
}

/** Scheduler that defines the basic data-types available to the user and creates turns for propagation handling.
  * Note: This should NOT extend [[DynamicScope]], but did so in the past and there are too many tests that assume so ...
  */
@implicitNotFound(msg = "Could not find an implicit scheduler. Did you forget an import?")
trait Scheduler[State[_]] extends DynamicScope[State] {
  final def forceNewTransaction[R](initialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => R)
      : R = {
    forceNewTransaction(initialWrites.toSet, admissionPhase)
  }
  def forceNewTransaction[R](initialWrites: Set[ReSource.of[State]], admissionPhase: AdmissionTicket[State] => R): R
  private[rescala] def singleReadValueOnce[A](reactive: ReadAs.of[State, A]): A

  /** Name of the scheduler, used for helpful error messages. */
  def schedulerName: String
  override def toString: String = s"Scheduler($schedulerName)"

  def maybeTransaction: Option[Transaction.of[State]]
}

/** Provides the capability to look up transactions in the dynamic scope. */
trait DynamicScope[State[_]] {
  private[rescala] def dynamicTransaction[T](f: Transaction.of[State] => T): T
  def maybeTransaction: Option[Transaction.of[State]]
}

trait SchedulerImpl[State[_], Tx <: Transaction.of[State]] extends DynamicScope[State] with Scheduler[State] {

  final private[rescala] def dynamicTransaction[T](f: Transaction.of[State] => T): T = {
    _currentTransaction.value match {
      case Some(transaction) => f(transaction)
      case None              => forceNewTransaction(Set.empty, ticket => f(ticket.tx))
    }
  }

  final protected val _currentTransaction: DynamicVariable[Option[Tx]] =
    new DynamicVariable[Option[Tx]](None)
  final private[rescala] def withDynamicInitializer[R](init: Tx)(thunk: => R): R =
    _currentTransaction.withValue(Some(init))(thunk)

  final override def maybeTransaction: Option[Transaction.of[State]] = {
    _currentTransaction.value
  }
}

case class ScopeSearch[State[_]](self: Either[Transaction.of[State], DynamicScope[State]]) {

  /** Either just use the statically found transaction,
    * or do a lookup in the dynamic scope.
    * If the lookup fails, it will start a new transaction.
    */
  def embedTransaction[T](f: Transaction.of[State] => T): T =
    self match {
      case Left(integrated) => f(integrated)
      case Right(ds)        => ds.dynamicTransaction(dt => f(dt))
    }

  def maybeTransaction: Option[Transaction.of[State]] = self match {
    case Left(integrated) => Some(integrated)
    case Right(ds)        => ds.maybeTransaction
  }

}

/** As reactives can be created during propagation, any Ticket can be converted to a creation ticket. */
object ScopeSearch extends LowPriorityScopeImplicits {

  implicit def fromTicketImplicit[S[_]](implicit ticket: StaticTicket[S]): ScopeSearch[S] =
    new ScopeSearch(Left(ticket.tx))
  implicit def fromAdmissionImplicit[S[_]](implicit ticket: AdmissionTicket[S]): ScopeSearch[S] =
    new ScopeSearch(Left(ticket.tx))
  implicit def fromTransactionImplicit[S[_]](implicit tx: Transaction.of[S]): ScopeSearch[S] =
    new ScopeSearch(Left(tx))
}

/** If no Fitting Ticket is found, then these implicits will search for a [[DynamicScope]],
  * creating the reactives outside of any turn.
  */
sealed trait LowPriorityScopeImplicits {
  implicit def fromSchedulerImplicit[S[_]](implicit factory: DynamicScope[S]): ScopeSearch[S] =
    new ScopeSearch(Right(factory))
}
