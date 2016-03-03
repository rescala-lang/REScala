package rescala.propagation

import java.util

import rescala.graph.{Buffer, PropagationStruct, Pulse}

import scala.util.control.NonFatal

trait CommonPropagationImpl[S <: PropagationStruct] extends AbstractPropagation[S] {
  private val toCommit = new util.HashSet[Committable]()
  private val observers = new java.util.ArrayList[() => Unit]()
  override def schedule(commitable: Committable): Unit = toCommit.add(commitable)

  override def observe(f: => Unit): Unit = observers.add(f _)


  def commitPhase() = {
    val it = toCommit.iterator()
    while (it.hasNext) it.next().commit(this)
  }

  def rollbackPhase() = {
    val it = toCommit.iterator()
    while (it.hasNext) it.next().release(this)
  }

  def observerPhase() = {
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

  override def pulses[P](budP: S#SporeP[P, _]): Buffer[Pulse[P]] = budP.pulses
  override def incoming[R](bud: S#Spore[R]): Set[R] = bud.incoming(this)
  override def updateIncoming[R](bud: S#Spore[R], newDependencies: Set[R]): Unit = bud.updateIncoming(newDependencies)(this)

}
