package rescala.scheduler.twoversion

import rescala.core._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/** Basic implementation of the most fundamental propagation steps as defined by AbstractPropagation.
  * Only compatible with spore definitions that store a pulse value and support graph operations.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait TwoVersionTransactionImpl[S <: TwoVersionStruct] extends TwoVersionTransaction[S] with Initializer[S] {

  val token: Token = Token()

  val toCommit  = ArrayBuffer[ReSource[S]]()
  val observers = ArrayBuffer[Observation]()

  override def schedule(commitable: ReSource[S]): Unit = toCommit += commitable

  def observe(f: Observation): Unit = observers += f

  override def commitPhase(): Unit = toCommit.foreach { r => r.state.commit(r.commit) }

  override def rollbackPhase(): Unit = toCommit.foreach(r => r.state.release())

  override def observerPhase(): Unit = {
    var failure: Throwable = null
    observers.foreach { n =>
      try n.execute()
      catch { case NonFatal(e) => failure = e }
    }
    // find some failure and rethrow the contained exception
    // we should probably aggregate all of the exceptions,
    // but this is not the place to invent exception aggregation
    if (failure != null) throw failure
  }

  def prepareInitialChange(ic: InitialChange[S]): Unit

  final override def initializationPhase(initialChanges: Map[ReSource[S], InitialChange[S]]): Unit =
    initialChanges.values.foreach(prepareInitialChange)

  final def commitDependencyDiff(node: Derived[S], current: Set[ReSource[S]])(updated: Set[ReSource[S]]): Unit = {
    val indepsRemoved = current -- updated
    val indepsAdded   = updated -- current
    indepsRemoved.foreach(drop(_, node))
    indepsAdded.foreach(discover(_, node))
    writeIndeps(node, updated)
  }

  private[rescala] def discover(source: ReSource[S], sink: Derived[S]): Unit = source.state.discoveredBy(sink)
  private[rescala] def drop(source: ReSource[S], sink: Derived[S]): Unit     = source.state.droppedBy(sink)

  private[rescala] def writeIndeps(node: Derived[S], indepsAfter: Set[ReSource[S]]): Unit =
    node.state.updateIncoming(indepsAfter)

  /** allow the propagation to handle dynamic access to reactives */
  def beforeDynamicDependencyInteraction(dependency: ReSource[S]): Unit

  override private[rescala] def makeAdmissionPhaseTicket(initialWrites: Set[ReSource[S]]): AdmissionTicket[S] =
    new AdmissionTicket[S](this, initialWrites) {
      override private[rescala] def access(reactive: ReSource[S]): reactive.Value = {
        beforeDynamicDependencyInteraction(reactive)
        reactive.state.base(token)
      }
    }
  private[rescala] def makeDynamicReevaluationTicket[V, N](b: V): ReevTicket[V, S] =
    new ReevTicket[V, S](this, b) {
      override def dynamicAccess(reactive: ReSource[S]): reactive.Value =
        TwoVersionTransactionImpl.this.dynamicAfter(reactive)
      override def staticAccess(reactive: ReSource[S]): reactive.Value = reactive.state.get(token)
    }

  override def accessTicket(): AccessTicket[S] =
    new AccessTicket[S] {
      override def access(reactive: ReSource[S]): reactive.Value = TwoVersionTransactionImpl.this.dynamicAfter(reactive)
    }

  private[rescala] def dynamicAfter[P](reactive: ReSource[S]): reactive.Value = {
    // Note: This only synchronizes reactive to be serializable-synchronized, but not glitch-free synchronized.
    // Dynamic reads thus may return glitched values, which the reevaluation handling implemented in subclasses
    // must account for by repeating glitched reevaluations!
    beforeDynamicDependencyInteraction(reactive)
    reactive.state.get(token)
  }
  def writeState(pulsing: ReSource[S])(value: pulsing.Value): Unit = {
    if (pulsing.state.write(value, token)) this.schedule(pulsing)
  }

}
