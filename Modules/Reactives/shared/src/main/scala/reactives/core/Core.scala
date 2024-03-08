package reactives.core

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
  final type Rout = Result[State, Value]

  /** called if any of the dependencies ([[reactives.core.Core.ReSource]]s) changed in the current update turn,
    * after all (known) dependencies are updated
    */
  protected[reactives] def reevaluate(input: ReIn): Rout
}
object Derived { type of[S[_]] = Derived { type State[V] = S[V] } }

/** Allows converting the internal [[Value]] of the resource to an external [[A]] meant for consumers.
  * Generally, everything that reads a reactive, such as macros or .now, will make use of this.
  */
trait ReadAs[+A] extends ReSource {

  /** Interprets the internal type to the external type */
  def read(v: Value): A
}
object ReadAs { type of[S[_], A] = ReadAs[A] { type State[V] = S[V] } }

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

/** Result of a reevaluation */
trait Result[S[_], T] {

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
  def inputs(): Option[Set[ReSource.of[S]]]
}

/** Records side effects for later execution. */
trait Observation { def execute(): Unit }

/** Enables the creation of other reactives */
@implicitNotFound(msg = "Could not find capability to create reactives. Maybe a missing import?")
final class CreationTicket[State[_]](val scope: CreationScope[State], val info: ReInfo)

object CreationTicket {
  given fromScope[State[_]](using scope: CreationScope[State], line: ReInfo): CreationTicket[State] =
    new CreationTicket(scope, line)
  // cases below are when one explicitly passes one of the parameters
  implicit def fromTransaction[S[_]](tx: Transaction[S])(using line: ReInfo): CreationTicket[S] =
    new CreationTicket(CreationScope.StaticCreationScope(tx), line)
  implicit def fromName[State[_]](str: String)(using
      scopeSearch: CreationScope[State],
      info: ReInfo
  ): CreationTicket[State] =
    new CreationTicket(scopeSearch, info.derive(str))
}

final class CreationTicketCont[State[_]](ct: CreationTicket[State])
object CreationTicketCont {
  given fromTicket[State[_]](using ct: CreationTicket[State]): CreationTicketCont[State] =  CreationTicketCont(ct)
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

  final def forceNewTransaction[R](initialWrites: ReSource.of[S]*)(admissionPhase: AdmissionTicket[S] => R): R = {
    forceNewTransaction(initialWrites.toSet, admissionPhase)
  }
  def forceNewTransaction[R](initialWrites: Set[ReSource.of[S]], admissionPhase: AdmissionTicket[S] => R): R
  private[reactives] def singleReadValueOnce[A](reactive: ReadAs.of[S, A]): A

  /** Name of the scheduler, used for helpful error messages. */
  def schedulerName: String
  override def toString: String = s"Scheduler($schedulerName)"
}
object Scheduler {
  given implicitScheduler: Scheduler[reactives.default.global.State] = reactives.default.global.scheduler
}
