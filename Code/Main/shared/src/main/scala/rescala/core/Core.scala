package rescala.core

import rescala.operator.RExceptions

import scala.annotation.implicitNotFound
import scala.util.DynamicVariable

trait Core {
  type State[_]

  /** Source of (reactive) values. */
  trait ReSource {
    type Value
    protected[rescala] def state: State[Value]
    protected[rescala] def name: ReName
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
  trait Readable[+A] extends ReSource {

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
        needsReevaluation: Boolean,
        creationTicket: CreationTicket
    )(instantiateReactive: State[V] => T): T = {
      val state    = makeDerivedStructState[V](initValue)
      val reactive = instantiateReactive(state)
      register(reactive)
      initialize(reactive, incoming, needsReevaluation)
      reactive
    }

    /** hook for schedulers to globally collect all created resources, usually does nothing */
    protected[this] def register(reactive: ReSource): Unit = ()

    /** Correctly initializes [[ReSource]]s */
    final private[rescala] def createSource[V, T <: ReSource](
        intv: V,
        creationTicket: CreationTicket
    )(instantiateReactive: State[V] => T): T = {
      val state    = makeSourceStructState[V](intv)
      val reactive = instantiateReactive(state)
      register(reactive)
      reactive
    }

    /** Creates the internal state of [[Derived]]s */
    protected[this] def makeDerivedStructState[V](valuePersistency: V): State[V]

    /** Creates the internal state of [[ReSource]]s */
    protected[this] def makeSourceStructState[V](valuePersistency: V): State[V] =
      makeDerivedStructState[V](valuePersistency)

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
    final def dependStatic[A](reactive: Readable[A]): A = reactive.read(collectStatic(reactive))
  }

  /** User facing low level API to access values in a dynamic context. */
  abstract class DynamicTicket(tx: Transaction) extends StaticTicket(tx) {
    private[rescala] def collectDynamic(reactive: ReSource): reactive.Value
    final def depend[A](reactive: Readable[A]): A = reactive.read(collectDynamic(reactive))
  }

  /** [[ReevTicket]] is given to the [[Derived]] reevaluate method and allows to access other reactives.
    * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
    * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
    */
  abstract class ReevTicket[V](tx: Transaction, private var _before: V)
      extends DynamicTicket(tx)
      with Result[V] {

    // schedulers implement these to allow access
    protected def staticAccess(reactive: ReSource): reactive.Value
    protected def dynamicAccess(reactive: ReSource): reactive.Value

    // dependency tracking accesses
    private[rescala] final override def collectStatic(reactive: ReSource): reactive.Value = {
      assert(collectedDependencies == null || collectedDependencies.contains(reactive))
      staticAccess(reactive)
    }

    private[rescala] final override def collectDynamic(reactive: ReSource): reactive.Value = {
      assert(collectedDependencies != null, "may not access dynamic dependencies without tracking dependencies")
      val updatedDeps = collectedDependencies + reactive
      if (updatedDeps eq collectedDependencies) {
        staticAccess(reactive)
      } else {
        collectedDependencies = updatedDeps
        dynamicAccess(reactive)
      }
    }

    // inline result into ticket, to reduce the amount of garbage during reevaluation
    private var collectedDependencies: Set[ReSource] = null

    private var _propagate          = false
    private var value: V            = _
    private var effect: Observation = null
    override final def toString: String =
      s"Result(value = $value, propagate = $activate, deps = $collectedDependencies)"
    final def before: V = _before

    /** Advises the ticket to track dynamic dependencies.
      * The passed initial set of dependencies may be processed as if they were static,
      * and are also returned in the resulting dependencies.
      */
    final def trackDependencies(initial: Set[ReSource]): ReevTicket[V] = { collectedDependencies = initial; this }
    final def trackStatic(): ReevTicket[V]                             = { collectedDependencies = null; this }
    final def withPropagate(p: Boolean): ReevTicket[V]                 = { _propagate = p; this }
    final def withValue(v: V): ReevTicket[V] = {
      require(v != null, "value must not be null");
      value = v;
      _propagate = true;
      this
    }
    final def withEffect(v: Observation): ReevTicket[V] = { effect = v; this }

    final override def activate: Boolean                       = _propagate
    final override def forValue(f: V => Unit): Unit            = if (value != null) f(value)
    final override def forEffect(f: Observation => Unit): Unit = if (effect != null) f(effect)
    final override def inputs(): Option[Set[ReSource]]         = Option(collectedDependencies)

    final def reset[NT](nb: NT): ReevTicket[NT] = {
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
      * None for static reactives.
      * Otherwise a list of all static reactives, and accessed dynamic reactives.
      */
    def inputs(): Option[Set[ReSource]]
  }

  /** Records side effects for latex execution. */
  trait Observation { def execute(): Unit }

  /** Enables reading of the current value during admission.
    * Keeps track of written sources internally.
    */
  final class AdmissionTicket(val tx: Transaction, declaredWrites: Set[ReSource]) {

    private var _initialChanges: Map[ReSource, InitialChange]         = Map[ReSource, InitialChange]()
    private[rescala] def initialChanges: Map[ReSource, InitialChange] = _initialChanges
    private[rescala] def recordChange[T](ic: InitialChange): Unit = {
      assert(
        declaredWrites.contains(ic.source),
        "must not set a source that has not been pre-declared for the transaction"
      )
      assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
      _initialChanges += ic.source -> ic
    }

    /** convenience method as many case studies depend on this being available directly on the AT */
    def now[A](reactive: Readable[A]): A = tx.now(reactive)

    private[rescala] var wrapUp: Transaction => Unit = null
  }

  /** Enables the creation of other reactives */
  @implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
  final class CreationTicket(val self: Either[Initializer, Scheduler], val rename: ReName) {

    private[rescala] def create[V, T <: Derived](
        incoming: Set[ReSource],
        initValue: V,
        needsReevaluation: Boolean
    )(instantiateReactive: State[V] => T): T = {
      dynamicCreation(_.create(incoming, initValue, needsReevaluation, this)(instantiateReactive))
    }
    private[rescala] def createSource[V, T <: ReSource](intv: V)(instantiateReactive: State[V] => T): T = {
      dynamicCreation(_.createSource(intv, this)(instantiateReactive))
    }

    /** Using the ticket requires to create a new scope, such that we can ensure that everything happens in the same transaction */
    def dynamicCreation[T](f: Initializer => T): T =
      self match {
        case Left(integrated) => f(integrated)
        case Right(engine)    => engine.dynamicTransaction(dt => f(dt.initializer))
      }
  }

  /** As reactives can be created during propagation, any Ticket can be converted to a creation ticket. */
  object CreationTicket extends LowPriorityCreationImplicits {
    implicit def fromTicketImplicit(implicit ticket: StaticTicket, line: ReName): CreationTicket =
      new CreationTicket(Left(ticket.tx.initializer), line)
    implicit def fromAdmissionImplicit(implicit ticket: AdmissionTicket, line: ReName): CreationTicket =
      new CreationTicket(Left(ticket.tx.initializer), line)
    implicit def fromInitializerImplicit(implicit initializer: Initializer, line: ReName): CreationTicket =
      new CreationTicket(Left(initializer), line)
    implicit def fromInitializer(creation: Initializer)(implicit line: ReName): CreationTicket =
      new CreationTicket(Left(creation), line)
    implicit def fromTransactionImplicit(implicit tx: Transaction, line: ReName): CreationTicket =
      new CreationTicket(Left(tx.initializer), line)
  }

  /** If no Fitting Ticket is found, then these implicits will search for a [[Scheduler]],
    * creating the reactives outside of any turn.
    */
  sealed trait LowPriorityCreationImplicits {
    implicit def fromSchedulerImplicit(implicit factory: Scheduler, line: ReName): CreationTicket =
      new CreationTicket(Right(factory), line)
    implicit def fromScheduler(factory: Scheduler)(implicit line: ReName): CreationTicket =
      new CreationTicket(Right(factory), line)
    implicit def fromNameImplicit(line: String)(implicit outer: CreationTicket): CreationTicket =
      new CreationTicket(outer.self, line)
  }

  /** Essentially a kill switch, that will remove the reactive at some point. */
  trait Disconnectable {
    def disconnect()(implicit engine: Scheduler): Unit
  }

  /** Removes the reactive instead of its next normal reevaluation.
    * This makes use of the fact, that all reactives are technically dynamic reactives,
    * and removing incoming dependencies is always kinda safe, as long as we are sure we no longer care!
    */
  trait DisconnectableImpl extends Derived with Disconnectable {
    @volatile private var disconnected = false
    final def disconnect()(implicit engine: Scheduler): Unit = {
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

    final def now[A](reactive: Readable[A]): A = {
      RExceptions.toExternalReadException(reactive, reactive.read(access(reactive)))
    }
    private[rescala] def access(reactive: ReSource): reactive.Value

    def initializer: Initializer
  }

  /** Scheduler that defines the basic data-types available to the user and creates turns for propagation handling */
  @implicitNotFound(msg = "Could not find an implicit scheduler. Did you forget an import?")
  trait Scheduler {
    final def forceNewTransaction[R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => R): R = {
      forceNewTransaction(initialWrites.toSet, admissionPhase)
    }
    def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R
    private[rescala] def singleReadValueOnce[A](reactive: Readable[A]): A
    private[rescala] def dynamicTransaction[T](f: Transaction => T): T

    /** Name of the scheduler, used for helpful error messages. */
    def schedulerName: String
    override def toString: String = s"Scheduler($schedulerName)"
  }

  trait SchedulerImpl[Tx <: Transaction] extends Scheduler {

    final override private[rescala] def dynamicTransaction[T](f: Transaction => T): T = {
      _currentInitializer.value match {
        case Some(transaction) => f(transaction)
        case None              => forceNewTransaction(Set.empty, ticket => f(ticket.tx))
      }
    }

    final protected val _currentInitializer: DynamicVariable[Option[Tx]] =
      new DynamicVariable[Option[Tx]](None)
    final private[rescala] def withDynamicInitializer[R](init: Tx)(thunk: => R): R =
      _currentInitializer.withValue(Some(init))(thunk)
  }
}
