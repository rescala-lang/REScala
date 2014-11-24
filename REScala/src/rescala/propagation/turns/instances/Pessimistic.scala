package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.{LockOwner, TurnLock}



class Pessimistic extends AbstractTurn with LockOwner {

  implicit def lockOwner: Pessimistic = this


  /** check if the current turn hold the lock */
  override def checkLock(lock: TurnLock): Boolean = {
    lock.isAccessible || lockOwner.heldLocks.isEmpty || lock.reactive.dependants.getU.exists(_.lock.isAccessible)
  }


  /** changed is called whenever the turn does anything to a reactive that needs to be commited
    * it is overridden here to detect changes to reactive which are not locked
    * this allows to detect errors early, but should never happen if the locking strategy is correct */
  override def markForCommit(reactive: Reactive): Unit = {
    if (!reactive.lock.isAccessible)
      throw new IllegalStateException(s"tried to change reactive $reactive but is locked by someone else")
    super.markForCommit(reactive)
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
    super.create(dependencies)(reactive)
  }

  /** similar to create, except for the ensure level and evaluate calls */
  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock()
    super.createDynamic(dependencies)(reactive)
  }

  /** a dynamically acquired reactive will never have its value or level changed, only its dependecies.
    * as from a conceptual level it is only read.
    * TODO: actually, we need to do something in the case where we access a shared lock and change its dependencies,
    * TODO: if that reactive had its value already changed by the owning transaction
    * TODO: basically, the dynamic acquisition is only used for adding dependecies
    * TODO: so the owning transaction needs to be informed that the added dependency needs to be evaluated as well
    * TODO: (we also use dynamic acquisition for removing dependencies, but to write those one also needs the lock on
    * TODO: the old dependency, which we already have, so no write conflicts there.
    *
    * the check for accessibilty is not strictly necessary as request will handle that as well,
    * which is necessary, because accessibility state may change between the call to isAccessible and request
    * if the current owner of the lock decides to share it with us.
    * but that case seems very unlikely, so the test should provide a solid shortcut */
  def acquireDynamic(reactive: Reactive): Unit = {
    if (!reactive.lock.isAccessible) {
      reactive.lock.request()
    }
    if (reactive.lock.isShared) {
      //TODO: somehow tell the owner that we did something to one of its reactives
      //TODO: maybe make turn state for dependencies smarter?
    }
  }
  /** admits a new source change */
  override def admit(source: Reactive)(setPulse: => Boolean): Unit = {
    source.lock.lock()
    super.admit(source)(setPulse)
  }


  /** TODO: this probably needs improvement … well it definitely does
    * TODO: the problem is, that lockOrdered tries to lock the reactive,
    * TODO: which does not consider shared locks.
    * TODO: so we might actually run into problems if someone tries to share a lock with us
    * TODO: while we do our initial locking …
    * tried to solve this by acquiring the master lock during initial locking,
    * so that nothing can be shared with us.
    * this still has problems, because evaluating the initial closure of the turn may create new reactives,
    * which causes dynamic locking to happen and screw us here. */
  def lockReachable(): Unit = {
    def reachable(reactives: Set[Reactive]): Set[Reactive] =
      reactives ++ reactives.flatMap(r => reachable(r.dependants.getU))

    masterLock.lock()
    try {
      val sources = initialSources
      lockOrdered(sources)
      val locked = reachable(sources.toSet).toSeq
      lockOrdered(locked)
      //TODO: need to check if the dependencies have changed in between
      //TODO: … it might actually be better to lock directly and always do deadlock detection
    }
    finally {
      masterLock.unlock()
    }
  }

  /** helper to lock a sequence of reactives in a given order to prevent deadlocks */
  final def lockOrdered(reactives: Seq[Reactive])(implicit turn: LockOwner): Unit = reactives.sortBy(System.identityHashCode).foreach { _.lock.lock() }


  /** this is called after the initial closure of the turn has been executed,
    * that is the eval queue is populated with the sources */
  override def lockingPhase(): Unit = lockReachable()
  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def realeasePhase(): Unit = releaseAll()
}

