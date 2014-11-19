package rescala.propagation.turns

import rescala.propagation.{LockOwner, Reactive}


object Pessimistic extends AbstractTurnFactory[Pessimistic](() => new Pessimistic()) {

  /** this probably needs improvement … well it definitely does */
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

  /** this is called after the initial closure of the turn has been executed,
    * that is the eval queue is populated with the sources*/
  override def acquirePreTurnLocks(turn: Pessimistic): Unit = lockReachable(turn)
  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releaseAllLocks(turn: Pessimistic): Unit = turn.releaseAll()

}

class Pessimistic extends AbstractTurn with LockOwner {

  implicit def lockOwner: Pessimistic = this

  /** changed is called whenever the turn does anything to a reactive that needs to be commited
    * it is overridden here to detect changes to reactive which are not locked
    * this allows to detect errors early, but should never happen if the locking strategy is correct */
  override def changed(reactive: Reactive): Unit = {
    if (!reactive.lock.isAccessible)
      throw new IllegalStateException(s"tried to change reactive $reactive but is locked by someone else")
    super.changed(reactive)
  }

  /** creating a signal causes some unpredictable reactives to be used inside the turn.
    * these will have their locks be acquired dynamically see below for how that works.
    * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
    * (we do need to grab it, so it can be transferred to some other waiting transaction).
    * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
    * is executed, because the constructor typically accesses the dependencies to create its initial value. */
  override def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock()
    register(reactive, dependencies)
    reactive
  }

  /** similar to create, except for the ensure level and evaluate calls */
  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock()
    ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }

  /** the check for accessibilty is not strictly necessary as request will handle that as well,
    * which is necessary, because accessibility state may change between the call to isAccessible and request
    * if the current owner of the lock decides to share it with us.
    * but that case seems very unlikely, so the test should provide a solid shortcut */
  def acquireDynamic(reactive: Reactive): Unit = {
    if (!reactive.lock.isAccessible) {
      reactive.lock.request()
    }
  }

}

