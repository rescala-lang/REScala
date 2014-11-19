package rescala.propagation.turns

import rescala.propagation.{LockOwner, Reactive}


object Pessimistic extends AbstractTurnFactory[Pessimistic](() => new Pessimistic()) {

  def lockReachable(turn: Pessimistic): Unit = {
    def reachable(reactives: Set[Reactive])(implicit turn: Pessimistic): Set[Reactive] =
      reactives ++ reactives.flatMap(r => reachable(r.dependants.get))

    val sources = turn.evalQueue.map(_._2).toSeq
    turn.lockOrdered(sources.map(_.lock))
    val locked = reachable(sources.toSet)(turn).toSeq
    turn.lockOrdered(locked.map(_.lock))
    //TODO: need to check if the dependencies have changed in between
    //TODO: … it might actually be better to lock directly and always do deadlock detection
  }

  override def acquirePreTurnLocks(turn: Pessimistic): Unit = lockReachable(turn)
  override def releaseAllLocks(turn: Pessimistic): Unit = turn.releaseAll()

}

class Pessimistic extends AbstractTurn with LockOwner {

  implicit def lockOwner: Pessimistic = this

  /** changed is called whenever the turn does anything to a reactive that needs to be commited
    * it is overridden here to detect changes to reactive which are not locked*/
  override def changed(reactive: Reactive): Unit = {
    if (!reactive.lock.isAccessible)
      throw new IllegalStateException(s"tried to change reactive $reactive but is locked by someone else")
    super.changed(reactive)
  }
  override def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock()
    register(reactive, dependencies)
    reactive
  }

  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock()
    ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }

  def acquireDynamic(reactive: Reactive): Unit = {
    if (!reactive.lock.isShared) {
      reactive.lock.request()
    }
  }

}

