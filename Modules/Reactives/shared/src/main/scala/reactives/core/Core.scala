package reactives.core

import reactives.core.DynamicScope.DynamicScopeImpl
import reactives.operator.Interface
import reactives.structure.RExceptions

import scala.annotation.implicitNotFound
import scala.util.DynamicVariable

/** Source of (reactive) values. */
trait ReSource {

  /** The type of the time-changing `Value` contained in this `ReSource` */
  type Value

  /** Additional structure required by schedulers for their propagation.
    * For example, outgoing dependencies, multi-versioned values, locks.
    */
  type State[_]

  /** The value of a resource is “protected” within the state.
    * A one of the access tickets available during a transaction is required to access the value.
    */
  protected[reactives] def state: State[Value]

  /** Converts the `base` value that is used during the transaction, to the value stored outside the transaction
    * This default implementation does not modify the value.
    * This default provides [[reactives.operator.Signal]] like semantics:
    * The final value during a transaction remains available outside of a transaction.
    * [[reactives.operator.Event]]s override this to reset to their “no value” state.
    */
  protected[reactives] def commit(base: Value): Value = base

  /** Developer friendly information about the resource. */
  def info: ReInfo
}
// we could replace this pattern by just a type operator for all types, but currently does not seem worth it
// infix type of[R <: ReSource, S[_]] = R {type State[A] = S[A]}
object ReSource { type of[S[_]] = ReSource { type State[V] = S[V] } }

/** A reactive value is something that can be reevaluated */
trait Derived extends ReSource {

  final type ReIn = ReevTicket[State, Value]
  final type Rout = Result.of[State, Value]

  /** called if any of the dependencies ([[reactives.core.Core.ReSource]]s) changed in the current update turn,
    * after all (known) dependencies are updated
    */
  protected[reactives] def reevaluate(input: ReIn): Rout
}
object Derived { type of[S[_]] = Derived { type State[V] = S[V] } }

/** Base implementation for reactives, with [[Derived]] for scheduling,
  * together with a [[ReInfo]] and containing a [[State]]
  *
  * @param state the state passed by the scheduler
  * @param info  the name of the reactive, useful for debugging as it often contains positional information
  */
abstract class Base[V](
    override protected[reactives] val state: reactives.operator.Interface.State[V],
    override val info: ReInfo
) extends ReSource {

  override type State[V] = reactives.operator.Interface.State[V]
  override type Value    = V
  override def toString: String = s"${info.description}($state)"
}

/** Common macro accessors for [[reactives.operator.Signal]] and [[reactives.operator.Event]]
  *
  * @tparam A return type of the accessor
  * @groupname accessor Accessor and observers
  */
trait ReadAs[+A] extends ReSource {

  /** Interprets the internal type to the external type
    *
    * @group internal
    */
  def read(v: Value): A
}
object ReadAs { type of[S[_], A] = ReadAs[A] { type State[V] = S[V] } }

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
trait Initializer[S[_]] {

  /** Creates and correctly initializes new [[Derived]]s */
  final private[reactives] def create[V, T <: Derived.of[S]](
      incoming: Set[ReSource.of[S]],
      initialValue: V,
      needsReevaluation: Boolean
  )(instantiateReactive: S[V] => T): T = {
    val state    = makeDerivedStructState[V](initialValue)
    val reactive = instantiateReactive(state)
    register(reactive, incoming, initialValue)
    initialize(reactive, incoming, needsReevaluation)
    reactive
  }

  /** hook for schedulers to globally collect all created resources, usually does nothing */
  protected def register[V](reactive: ReSource.of[S], inputs: Set[ReSource.of[S]], initValue: V): Unit = {
    Tracing.observe(Tracing.Create(reactive, inputs.toSet, Tracing.ValueWrapper(initValue)))
  }

  /** Correctly initializes [[ReSource]]s */
  final private[reactives] def createSource[V, T <: ReSource.of[S]](initialValue: V)(instantiateReactive: S[V] => T)
      : T = {
    val state    = makeSourceStructState[V](initialValue)
    val reactive = instantiateReactive(state)
    register(reactive, Set.empty, initialValue)
    reactive
  }

  /** Creates the internal state of [[reactives.core.Derived]]s */
  protected def makeDerivedStructState[V](initialValue: V): S[V]

  /** Creates the internal state of [[ReSource]]s */
  protected def makeSourceStructState[V](initialValue: V): S[V] =
    makeDerivedStructState[V](initialValue)

  /** to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    *
    * @param reactive          the newly instantiated reactive
    * @param incoming          a set of incoming dependencies
    * @param needsReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected def initialize(
      reactive: Derived.of[S],
      incoming: Set[ReSource.of[S]],
      needsReevaluation: Boolean
  ): Unit

}

/** User facing low level API to access values in a static context. */
sealed abstract class StaticTicket[State[_]](val tx: Transaction[State]) {
  private[reactives] def collectStatic(reactive: ReSource.of[State]): reactive.Value
  final def dependStatic[A](reactive: ReadAs.of[State, A]): A = reactive.read(collectStatic(reactive))
}

/** User facing low level API to access values in a dynamic context. */
abstract class DynamicTicket[State[_]](tx: Transaction[State]) extends StaticTicket[State](tx) {
  private[reactives] def collectDynamic(reactive: ReSource.of[State]): reactive.Value
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
final class ReevTicket[S[_], V](tx: Transaction[S], private var _before: V, accessHandler: AccessHandler[S])
    extends DynamicTicket(tx)
    with Result[V] {

  override type State[V] = S[V]

  private var collectedDependencies: Set[ReSource.of[State]] = null

  // dependency tracking accesses
  private[reactives] override def collectStatic(reactive: ReSource.of[State]): reactive.Value = {
    assert(collectedDependencies == null || collectedDependencies.contains(reactive))
    accessHandler.staticAccess(reactive)
  }

  private[reactives] override def collectDynamic(reactive: ReSource.of[State]): reactive.Value = {
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
  private var value: V            = scala.compiletime.uninitialized
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
final class AdmissionTicket[State[_]](val tx: Transaction[State], declaredWrites: Set[ReSource.of[State]]) {

  private var _initialChanges: Map[ReSource.of[State], InitialChange[State]] =
    Map[ReSource.of[State], InitialChange[State]]()
  private[reactives] def initialChanges: Map[ReSource.of[State], InitialChange[State]] = _initialChanges
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

  private[reactives] var wrapUp: Transaction[State] => Unit = null
}

/** Enables the creation of other reactives */
@implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
final class CreationTicket[State[_]](val scope: CreationScope[State], val info: ReInfo)

object CreationTicket {
  implicit def fromScope[State[_]](implicit scope: CreationScope[State], line: ReInfo): CreationTicket[State] =
    new CreationTicket(scope, line)
  // cases below are when one explicitly passes one of the parameters
  implicit def fromTransaction[S[_]](tx: Transaction[S])(implicit line: ReInfo): CreationTicket[S] =
    new CreationTicket(CreationScope.StaticCreationScope(tx), line)
  implicit def fromName[State[_]](str: String)(implicit
      scopeSearch: CreationScope[State],
      info: ReInfo
  ): CreationTicket[State] =
    new CreationTicket(scopeSearch, info.derive(str))

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

  final override protected[reactives] def reevaluate(rein: ReIn): Rout = {
    if (disconnected) {
      rein.trackDependencies(Set.empty)
      rein
    } else {
      guardedReevaluate(rein)
    }
  }

  protected[reactives] def guardedReevaluate(rein: ReIn): Rout

}

/** A transaction (or maybe transaction handle would be the better term) is available from reevaluation and admission tickets.
  * That is, everywhere during a transaction, you can read reactives, but also create them.
  * The reading values is core to any reactive propagation.
  * But creating reactives using the [[Initializer]] is a liability to the scheduler, but a superpower to the operators.
  * Its a classical tradeoff, but it would be better to not make this choice by default,
  * that is, reactive creation should be limited such that we can experiment with schedulers that do not have this liability.
  */
trait Transaction[State[_]] {

  final def now[A](reactive: ReadAs.of[State, A]): A = {
    RExceptions.toExternalReadException(reactive, reactive.read(access(reactive)))
  }
  private[reactives] def access(reactive: ReSource.of[State]): reactive.Value

  def observe(obs: Observation): Unit

  def initializer: Initializer[State]

  private[reactives] def discover(source: ReSource.of[State], sink: Derived.of[State]): Unit = {
    Tracing.observe(Tracing.Discover(source, sink))
  }
  private[reactives] def drop(source: ReSource.of[State], sink: Derived.of[State]): Unit = {
    Tracing.observe(Tracing.Drop(source, sink))
  }
}

/** Scheduler that defines the basic data-types available to the user and creates turns for propagation handling. */
@implicitNotFound(msg = "Could not find an implicit scheduler. Did you forget an import?")
trait Scheduler[S[_]] {

  type SchedulerState[T] = S[T]

  final def forceNewTransaction[R](initialWrites: ReSource.of[S]*)(admissionPhase: AdmissionTicket[S] => R): R = {
    forceNewTransaction(initialWrites.toSet, admissionPhase)
  }
  def forceNewTransaction[R](initialWrites: Set[ReSource.of[S]], admissionPhase: AdmissionTicket[S] => R): R
  private[reactives] def singleReadValueOnce[A](reactive: ReadAs.of[S, A]): A

  /** Name of the scheduler, used for helpful error messages. */
  def schedulerName: String
  override def toString: String = s"Scheduler($schedulerName)"

  def dynamicScope: DynamicScope[S]
}
object Scheduler {
  given defaultScheduler: Scheduler[Interface.State] = Interface.default
}

trait SchedulerImpl[State[_], Tx <: Transaction[State]] extends Scheduler[State] {
  override def dynamicScope: DynamicScopeImpl[State, Tx] = new DynamicScopeImpl[State, Tx](this)
}

/** Provides the capability to look up transactions in the dynamic scope. */
trait DynamicScope[State[_]] {
  private[reactives] def dynamicTransaction[T](f: Transaction[State] => T): T
  def maybeTransaction: Option[Transaction[State]]
}

object DynamicScope {
  class DynamicScopeImpl[State[_], Tx <: Transaction[State]](scheduler: SchedulerImpl[State, Tx])
      extends DynamicScope[State] {

    final private[reactives] def dynamicTransaction[T](f: Transaction[State] => T): T = {
      _currentTransaction.value match {
        case Some(transaction) => f(transaction)
        case None              => scheduler.forceNewTransaction(Set.empty, ticket => f(ticket.tx))
      }
    }

    final protected val _currentTransaction: DynamicVariable[Option[Tx]] =
      new DynamicVariable[Option[Tx]](None)

    final private[reactives] def withDynamicInitializer[R](init: Tx)(thunk: => R): R =
      _currentTransaction.withValue(Some(init))(thunk)

    final override def maybeTransaction: Option[Tx] = {
      _currentTransaction.value
    }

  }
}
