package rescala.twoversion

import rescala.graph.ReevaluationResult.DepDiff
import rescala.graph.{Pulsing, Reactive}

import scala.util.control.NonFatal

/**
  * Basic implementation of the most fundamental propagation steps as defined by AbstractPropagation.
  * Only compatible with spore definitions that store a pulse value and support graph operations.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait CommonPropagationImpl[S <: GraphStruct] extends TwoVersionPropagation[S] {
  outer =>

  val token: Token = Token()

  private val toCommit = scala.collection.mutable.ArrayBuffer[Committable[S]]()
  private val observers = scala.collection.mutable.ArrayBuffer[() => Unit]()

  override def schedule(commitable: Committable[S]): Unit = toCommit += commitable

  override def observe(f: () => Unit): Unit = observers += f

  override def commitPhase(): Unit = {
    val it = toCommit.iterator
    while (it.hasNext) it.next().commit(this)
  }

  override def rollbackPhase(): Unit = {
    val it = toCommit.iterator
    while (it.hasNext) it.next().release(this)
  }

  override def observerPhase(): Unit = {
    val it = observers.iterator
    var failure: Throwable = null
    while (it.hasNext) {
      try {
        it.next().apply()
      }
      catch {
        case NonFatal(e) => failure = e
      }
    }
    // find the first failure and rethrow the contained exception
    // we should probably aggregate all of the exceptions,
    // but this is not the place to invent exception aggregation
    if (failure != null) throw failure
  }

  protected def discover(sink: Reactive[S])(source: Reactive[S]): Unit = source.state.discover(sink)(this)

  protected def drop(sink: Reactive[S])(source: Reactive[S]): Unit = source.state.drop(sink)(this)

  final def applyDiff(head: Reactive[S], diff: DepDiff[S]): Unit = {
    head.state.updateIncoming(diff.novel)(this)
    diff.removed foreach drop(head)
    diff.added foreach discover(head)
  }
  override def before[P](pulsing: Pulsing[P, S]): P = pulsing.state.base(token)
  override def after[P](pulsing: Pulsing[P, S]): P = pulsing.state.get(token)
  def writeState[P](pulsing: Reactive[S])(value: pulsing.Value): Unit = if (pulsing.state.write(value, token)) this.schedule(pulsing.state)
}
