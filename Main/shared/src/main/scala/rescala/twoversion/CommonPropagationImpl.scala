package rescala.twoversion

import rescala.graph.{ATicket, DepDiff, GraphStruct, Pulse, Reactive}
import rescala.propagation.{DynamicTicket, StaticTicket}

import scala.util.control.NonFatal

/**
  * Basic implementation of the most fundamental propagation steps as defined by AbstractPropagation.
  * Only compatible with spore definitions that store a pulse value and support graph operations.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait CommonPropagationImpl[S <: GraphStruct] extends AbstractPropagation[S] {
  outer =>

  def makeTicket(): S#Ticket[S] = new ATicket[S] {
    override def dynamic(): DynamicTicket[S] = new DynamicTicket[S](turn, this)
    override def static(): StaticTicket[S] = new StaticTicket[S](turn, this)
    override def turn(): CommonPropagationImpl[S] = outer
  }

  private val toCommit = new java.util.ArrayList[Committable[S]]()
  private val observers = new java.util.ArrayList[() => Unit]()

  override def schedule(commitable: Committable[S]): Unit = toCommit.add(commitable)

  override def observe(f: () => Unit): Unit = observers.add(f)

  def setIfChange(r: Reactive[S])(value: Option[r.Value])(implicit ticket: S#Ticket[S]): Boolean = {
    val changed = value.isDefined && value.get != r.state.base
    if (changed) r.state.set(value.get)
    changed
  }

  override def commitPhase(): Unit = {
    val it = toCommit.iterator()
    while (it.hasNext) it.next().commit(this)
  }

  override def rollbackPhase(): Unit = {
    val it = toCommit.iterator()
    while (it.hasNext) it.next().release(this)
  }

  override def observerPhase(): Unit = {
    val it = observers.iterator()
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


}
