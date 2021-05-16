package rescala.core

import rescala.operator.RExceptions

import scala.annotation.implicitNotFound
import scala.util.DynamicVariable

trait Core:
  type State[_]

  /** Source of (reactive) values, the [[Struct]] defines how the state is stored internally,
    * and how dependencies are managed.
    * State can only be accessed with a correct [[InnerTicket]].
    *
    * @tparam S [[Struct]] defining the internal state
    */
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

    /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
      * after all (known) dependencies are updated
      */
    protected[rescala] def reevaluate(input: ReIn): Rout
  }

  /** Base implementation for reactives, with [[Derived]] for scheduling,
    * together with a [[ReName]] and asking for a [[Struct.State]]
    *
    * @param state the initial state passed by the scheduler
    * @param name the name of the reactive, useful for debugging as it often contains positional information
    */
  abstract class Base[V](override protected[rescala] val state: State[V], override val name: ReName)
      extends ReSource {
    override type Value = V
    override def toString: String = s"${name.str}($state)"
  }

  /** Common macro accessors for [[rescala.operator.Signal]] and [[rescala.operator.Event]]
    * @tparam A return type of the accessor
    * @groupname accessor Accessor and observers
    */
  trait Interp[+A] extends ReSource {

    /** Interprets the internal type to the external type
      * @group internal
      */
    def interpret(v: Value): A
  }



  trait Initializer {

    /** Creates and correctly initializes new [[rescala.core.Derived]]s */
    final private[rescala] def create[V, T <: Derived](
        incoming: Set[ReSource],
        initv: V,
        inite: Boolean,
        creationTicket: CreationTicket
    )(instantiateReactive: State[V] => T): T = {
      val state    = makeDerivedStructState[V](initv)
      val reactive = instantiateReactive(state)
      register(reactive)
      ignite(reactive, incoming, inite)
      reactive
    }

    def accessTicket(): AccessTicket

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
      * @param reactive                     the newly instantiated reactive
      * @param incoming                     a set of incoming dependencies
      * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
      */
    protected[this] def ignite(
        reactive: Derived,
        incoming: Set[ReSource],
        ignitionRequiresReevaluation: Boolean
    ): Unit

  }



  /** [[InnerTicket]]s are used in Rescala to give capabilities to contexts during propagation.
    * [[ReevTicket]] is used during reevaluation, and [[AdmissionTicket]] during the initialization.
    */
  class InnerTicket(val initializer: Initializer)

  /** [[ReevTicket]] is given to the [[Derived]] reevaluate method and allows to access other reactives.
    * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
    * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
    */
  abstract class ReevTicket[V](initializer: Initializer, private var _before: V)
      extends DynamicTicket(initializer)
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

    private var _propagate              = false
    private var value: V                = _
    private var effect: Observation     = null
    override final def toString: String = s"Result(value = $value, propagate = $activate, deps = $collectedDependencies)"
    final def before: V                 = _before

    /** Advises the ticket to track dynamic dependencies.
      * The passed initial set of dependencies may be processed as if they were static,
      * and are also returned in the resulting dependencies.
      */
    final def trackDependencies(initial: Set[ReSource]): ReevTicket[V] = { collectedDependencies = initial; this }
    final def trackStatic(): ReevTicket[V] = { collectedDependencies = null; this }
    final def withPropagate(p: Boolean): ReevTicket[V] = { _propagate = p; this }
    final def withValue(v: V): ReevTicket[V] = {
      require(v != null, "value must not be null"); value = v; _propagate = true; this
    }
    final def withEffect(v: Observation): ReevTicket[V] = { effect = v; this }

    final override def activate: Boolean                       = _propagate
    final override def forValue(f: V => Unit): Unit            = if (value != null) f(value)
    final override def forEffect(f: Observation => Unit): Unit = if (effect != null) f(effect)
    final override def inputs(): Option[Set[ReSource]]      = Option(collectedDependencies)

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

  /** User facing low level API to access values in a dynamic context. */
  abstract class DynamicTicket(creation: Initializer) extends StaticTicket(creation) {
    private[rescala] def collectDynamic(reactive: ReSource): reactive.Value
    final def depend[A](reactive: Interp[A]): A = reactive.interpret(collectDynamic(reactive))
  }

  /** User facing low level API to access values in a static context. */
  sealed abstract class StaticTicket(creation: Initializer) extends InnerTicket(creation) {
    private[rescala] def collectStatic(reactive: ReSource): reactive.Value
    final def dependStatic[A](reactive: Interp[A]): A = reactive.interpret(collectStatic(reactive))
  }

  /** Encapsulates an action changing a single source. */
  trait InitialChange {

    /** The source to be changed. */
    val source: ReSource

    /** @param base the current (old) value of the source.
      * @param writeCallback callback to apply the new value, executed only if the action is approved by the source.
      * @return the propagation status of the source (whether or not to reevaluate output reactives).
      */
    def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean
  }

  /** Enables reading of the current value during admission.
    * Keeps track of written sources internally.
    */
  abstract class AdmissionTicket(initializer: Initializer, declaredWrites: Set[ReSource])
      extends InnerTicket(initializer)
      with AccessTicket {

    private var _initialChanges                                             = Map[ReSource, InitialChange]()
    private[rescala] def initialChanges: Map[ReSource, InitialChange] = _initialChanges
    private[rescala] def recordChange[T](ic: InitialChange): Unit = {
      assert(
        declaredWrites.contains(ic.source),
        "must not set a source that has not been pre-declared for the transaction"
      )
      assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
      _initialChanges += ic.source -> ic
    }

    private[rescala] var wrapUp: AccessTicket => Unit = null
  }

  trait AccessTicket {
    private[rescala] def access(reactive: ReSource): reactive.Value
    final def now[A](reactive: Interp[A]): A = {
      RExceptions.toExternalReadException(reactive, reactive.interpret(access(reactive)))
    }
  }

  /** Enables the creation of other reactives */
  @implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
  final case class CreationTicket(self: Either[Initializer, Scheduler], rename: ReName) {

    private[rescala] def create[V, T <: Derived](
        incoming: Set[ReSource],
        initv: V,
        inite: Boolean
    )(instantiateReactive: State[V] => T): T = {
      transaction(_.create(incoming, initv, inite, this)(instantiateReactive))
    }
    private[rescala] def createSource[V, T <: ReSource](intv: V)(instantiateReactive: State[V] => T): T = {

      transaction(_.createSource(intv, this)(instantiateReactive))
    }

    /** Returns true if this ticket is already part of a transaction. */
    def isInnerTicket(): Boolean = self.isLeft

    /** Using the ticket requires to create a new scope, such that we can ensure that everything happens in the same transaction */
    def transaction[T](f: Initializer => T): T =
      self match {
        case Left(integrated) => f(integrated)
        case Right(engine)    => engine.initializerDynamicLookup(f)
      }
  }

  /** As reactives can be created during propagation, any [[InnerTicket]] can be converted to a creation ticket. */
  object CreationTicket extends LowPriorityCreationImplicits {
    implicit def fromTicketImplicit(implicit ticket: InnerTicket, line: ReName): CreationTicket =
      CreationTicket(Left(ticket.initializer), line)

    implicit def fromInitializerImplicit(implicit
        initializer: Initializer,
        line: ReName
    ): CreationTicket = CreationTicket(Left(initializer), line)
    implicit def fromInitializer(creation: Initializer)(implicit line: ReName): CreationTicket =
      CreationTicket(Left(creation), line)
  }

  /** If no [[InnerTicket]] is found, then these implicits will search for a [[Scheduler]],
    * creating the reactives outside of any turn.
    */
  sealed trait LowPriorityCreationImplicits {
    implicit def fromSchedulerImplicit(implicit factory: Scheduler, line: ReName): CreationTicket =
      CreationTicket(Right(factory), line)
    implicit def fromScheduler(factory: Scheduler)(implicit line: ReName): CreationTicket =
      CreationTicket(Right(factory), line)
    implicit def fromNameImplicit(line: String)(implicit outer: CreationTicket): CreationTicket =
      CreationTicket(outer.self, line)
  }


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

  trait Observation {
    def execute(): Unit
  }

  trait Disconnectable {
    def disconnect()(implicit engine: Scheduler): Unit
  }

  trait DisconnectableImpl extends Derived with Disconnectable {
    @volatile private var disconnected = false
    final def disconnect()(implicit engine: Scheduler): Unit = {
      disconnected = true
    }

    def guardReevaluate(rein: ReIn)(normalEval: => Rout): Rout = {
      if (disconnected) {
        rein.trackDependencies(Set.empty)
        rein
      } else {
        normalEval
      }
    }

  }

  /** Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
    *
    * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
    */
  @implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
  trait Scheduler {
    final def forceNewTransaction[R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => R): R = {
      forceNewTransaction(initialWrites.toSet, admissionPhase)
    }
    def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R
    private[rescala] def initializerDynamicLookup[T](f: Initializer => T): T

    /** Name of the scheduler, used for helpful error messages. */
    def schedulerName: String
    override def toString: String = s"Scheduler($schedulerName)"
  }

  object Scheduler {
    def apply(implicit scheduler: Scheduler): Scheduler = scheduler
  }




  trait DynamicInitializerLookup[ExactInitializer <: Initializer] extends Scheduler {

    final override private[rescala] def initializerDynamicLookup[T](f: Initializer => T): T = {
      _currentInitializer.value match {
        case Some(turn) => f(turn)
        case None       => forceNewTransaction(Set.empty, ticket => f(ticket.initializer))
      }
    }

    final protected val _currentInitializer: DynamicVariable[Option[ExactInitializer]] =
      new DynamicVariable[Option[ExactInitializer]](None)
    final private[rescala] def withDynamicInitializer[R](init: ExactInitializer)(thunk: => R): R =
      _currentInitializer.withValue(Some(init))(thunk)
  }
