package rescala.core

import rescala.operator.RExceptions

import scala.annotation.{implicitNotFound, nowarn}
import scala.util.DynamicVariable

trait Core {

  /** In case you wondered why everything in REScala is in these weird bundle traits, this is why.
    * The ReSource below depends on some abstract state, which is defined by the concrete scheduler implementations.
    * As basically everything else references ReSources, everything must be bundled together.
    * This is good for users, because they get strong guarantees about the used correctness, and the API is still OK.
    * Its terrible for us, because the Scala Incremental compiler does not really work anymore.
    */
  type State[_]

  /** Source of (reactive) values. */
  trait ReSource {
    type Value
    protected[rescala] def state: State[Value]
    def name: ReName
    protected[rescala] def commit(base: Value): Value
  }

  /** A reactive value is something that can be reevaluated */
  trait Derived extends ReSource {

    final type ReIn = ReevTicket[Value]
    final type Rout = Result[Value]

    /** called if any of the dependencies ([[rescala.core.Core.ReSource]]s) changed in the current update turn,
      * after all (known) dependencies are updated
      */
    protected[rescala] def reevaluate(input: ReIn): Rout
  }

  /** Base implementation for reactives, with [[Derived]] for scheduling,
    * together with a [[ReName]] and containing a [[State]]
    *
    * @param state the initial state passed by the scheduler
    * @param name  the name of the reactive, useful for debugging as it often contains positional information
    */
  abstract class Base[V](override protected[rescala] val state: State[V], override val name: ReName)
      extends ReSource {
    override type Value = V
    override def toString: String = s"${name.str}($state)"
  }

  /** Common macro accessors for [[rescala.operator.SignalBundle.Signal]] and [[rescala.operator.EventBundle.Event]]
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

  /** Encapsulates an action changing a single source. */
  trait InitialChange {

    /** The source to be changed. */
    val source: ReSource

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

    /** Creates and correctly initializes new [[Derived]]s */
    final private[rescala] def create[V, T <: Derived](
        incoming: Set[ReSource],
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
    protected[this] def register(@nowarn("msg=never used") reactive: ReSource): Unit = ()

    /** Correctly initializes [[ReSource]]s */
    final private[rescala] def createSource[V, T <: ReSource](
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
        reactive: Derived,
        incoming: Set[ReSource],
        needsReevaluation: Boolean
    ): Unit

  }

  /** User facing low level API to access values in a static context. */
  sealed abstract class StaticTicket(val tx: Transaction) {
    private[rescala] def collectStatic(reactive: ReSource): reactive.Value
    final def dependStatic[A](reactive: ReadAs[A]): A = reactive.read(collectStatic(reactive))
  }

  /** User facing low level API to access values in a dynamic context. */
  abstract class DynamicTicket(tx: Transaction) extends StaticTicket(tx) {
    private[rescala] def collectDynamic(reactive: ReSource): reactive.Value
    final def depend[A](reactive: ReadAs[A]): A = reactive.read(collectDynamic(reactive))
  }

  trait AccessHandler {
    // schedulers implement these to allow access
    def staticAccess(reactive: ReSource): reactive.Value
    def dynamicAccess(reactive: ReSource): reactive.Value
  }

  /** [[ReevTicket]] is given to the [[Derived]] reevaluate method and allows to access other reactives.
    * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
    * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
    */
  final class ReevTicket[V](tx: Transaction, private var _before: V, accessHandler: AccessHandler)
      extends DynamicTicket(tx)
      with Result[V] {

    private var collectedDependencies: Set[ReSource] = null

    // dependency tracking accesses
    private[rescala] override def collectStatic(reactive: ReSource): reactive.Value = {
      assert(collectedDependencies == null || collectedDependencies.contains(reactive))
      accessHandler.staticAccess(reactive)
    }

    private[rescala] override def collectDynamic(reactive: ReSource): reactive.Value = {
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
    def trackDependencies(initial: Set[ReSource]): ReevTicket[V] = { collectedDependencies = initial; this }
    def trackStatic(): ReevTicket[V]                             = { collectedDependencies = null; this }
    def withPropagate(p: Boolean): ReevTicket[V]                 = { _propagate = p; this }
    def withValue(v: V): ReevTicket[V] = {
      require(v != null, "value must not be null");
      value = v;
      _propagate = true;
      this
    }
    def withEffect(v: Observation): ReevTicket[V] = { effect = v; this }

    override def activate: Boolean                       = _propagate
    override def forValue(f: V => Unit): Unit            = if (value != null) f(value)
    override def forEffect(f: Observation => Unit): Unit = if (effect != null) f(effect)
    override def inputs(): Option[Set[ReSource]]         = Option(collectedDependencies)

    def reset[NT](nb: NT): ReevTicket[NT] = {
      _propagate = false
      value = null.asInstanceOf[V]
      effect = null
      collectedDependencies = null
      val res = this.asInstanceOf[ReevTicket[NT]]
      res._before = nb
      res
    }
  }

  /** Result of a reevaluation */
  trait Result[T] {

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
    def inputs(): Option[Set[ReSource]]
  }

  /** Records side effects for later execution. */
  trait Observation { def execute(): Unit }

  /** Enables reading of the current value during admission.
    * Keeps track of written sources internally.
    */
  final class AdmissionTicket(val tx: Transaction, declaredWrites: Set[ReSource]) {

    private var _initialChanges: Map[ReSource, InitialChange]         = Map[ReSource, InitialChange]()
    private[rescala] def initialChanges: Map[ReSource, InitialChange] = _initialChanges
    def recordChange[T](ic: InitialChange): Unit = {
      assert(
        declaredWrites.contains(ic.source),
        "must not set a source that has not been pre-declared for the transaction"
      )
      assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
      _initialChanges += ic.source -> ic
    }

    /** convenience method as many case studies depend on this being available directly on the AT */
    def now[A](reactive: ReadAs[A]): A = tx.now(reactive)

    private[rescala] var wrapUp: Transaction => Unit = null
  }

  /** Enables the creation of other reactives */
  @implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
  final class CreationTicket(val scope: ScopeSearch, val rename: ReName) {

    private[rescala] def create[V, T <: Derived](
        incoming: Set[ReSource],
        initValue: V,
        needsReevaluation: Boolean
    )(instantiateReactive: State[V] => T): T = {
      scope.embedTransaction(_.initializer.create(incoming, initValue, needsReevaluation)(instantiateReactive))
    }
    private[rescala] def createSource[V, T <: ReSource](intv: V)(instantiateReactive: State[V] => T): T = {
      scope.embedTransaction(_.initializer.createSource(intv)(instantiateReactive))
    }
  }

  object CreationTicket {
    implicit def fromScope(implicit scope: ScopeSearch, line: ReName): CreationTicket =
      new CreationTicket(scope, line)
    // cases below are when one explicitly passes one of the parameters
    implicit def fromExplicitDynamicScope(factory: DynamicScope)(implicit line: ReName): CreationTicket =
      new CreationTicket(new ScopeSearch(Right(factory)), line)
    implicit def fromTransaction(tx: Transaction)(implicit line: ReName): CreationTicket =
      new CreationTicket(new ScopeSearch(Left(tx)), line)
    implicit def fromName(str: String)(implicit scopeSearch: ScopeSearch): CreationTicket =
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

    final def now[A](reactive: ReadAs[A]): A = {
      RExceptions.toExternalReadException(reactive, reactive.read(access(reactive)))
    }
    private[rescala] def access(reactive: ReSource): reactive.Value

    def observe(obs: Observation): Unit

    def initializer: Initializer
  }

  /** Scheduler that defines the basic data-types available to the user and creates turns for propagation handling.
    * Note: This should NOT extend [[DynamicScope]], but did so in the past and there are too many tests that assume so ...
    */
  @implicitNotFound(msg = "Could not find an implicit scheduler. Did you forget an import?")
  trait Scheduler extends DynamicScope {
    final def forceNewTransaction[R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => R): R = {
      forceNewTransaction(initialWrites.toSet, admissionPhase)
    }
    def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R
    private[rescala] def singleReadValueOnce[A](reactive: ReadAs[A]): A

    /** Name of the scheduler, used for helpful error messages. */
    def schedulerName: String
    override def toString: String = s"Scheduler($schedulerName)"

    def maybeTransaction: Option[Transaction]
  }

  /** Provides the capability to look up transactions in the dynamic scope. */
  trait DynamicScope {
    private[rescala] def dynamicTransaction[T](f: Transaction => T): T
    def maybeTransaction: Option[Transaction]
  }

  trait SchedulerImpl[Tx <: Transaction] extends DynamicScope with Scheduler {

    final private[rescala] def dynamicTransaction[T](f: Transaction => T): T = {
      _currentTransaction.value match {
        case Some(transaction) => f(transaction)
        case None              => forceNewTransaction(Set.empty, ticket => f(ticket.tx))
      }
    }

    final protected val _currentTransaction: DynamicVariable[Option[Tx]] =
      new DynamicVariable[Option[Tx]](None)
    final private[rescala] def withDynamicInitializer[R](init: Tx)(thunk: => R): R =
      _currentTransaction.withValue(Some(init))(thunk)

    final override def maybeTransaction: Option[Transaction] = {
      _currentTransaction.value
    }
  }

  case class ScopeSearch(self: Either[Transaction, DynamicScope]) {

    /** Either just use the statically found transaction,
      * or do a lookup in the dynamic scope.
      * If the lookup fails, it will start a new transaction.
      */
    def embedTransaction[T](f: Transaction => T): T =
      self match {
        case Left(integrated) => f(integrated)
        case Right(ds)        => ds.dynamicTransaction(dt => f(dt))
      }

    def maybeTransaction: Option[Transaction] = self match {
      case Left(integrated) => Some(integrated)
      case Right(ds)        => ds.maybeTransaction
    }

  }

  /** As reactives can be created during propagation, any Ticket can be converted to a creation ticket. */
  object ScopeSearch extends LowPriorityScopeImplicits {

    implicit def fromTicketImplicit(implicit ticket: StaticTicket): ScopeSearch =
      new ScopeSearch(Left(ticket.tx))
    implicit def fromAdmissionImplicit(implicit ticket: AdmissionTicket): ScopeSearch =
      new ScopeSearch(Left(ticket.tx))
    implicit def fromTransactionImplicit(implicit tx: Transaction): ScopeSearch =
      new ScopeSearch(Left(tx))
  }

  /** If no Fitting Ticket is found, then these implicits will search for a [[DynamicScope]],
    * creating the reactives outside of any turn.
    */
  sealed trait LowPriorityScopeImplicits {
    implicit def fromSchedulerImplicit(implicit factory: DynamicScope): ScopeSearch =
      new ScopeSearch(Right(factory))
  }
}
