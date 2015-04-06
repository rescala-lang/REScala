package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.TurnImpl

trait Prelock extends TurnImpl with InterturnDependencyChanges {

  override def toString: String = s"Prelock(${ id })"

  final val thread = Thread.currentThread().getName

  /**
   * creating a signal causes some unpredictable reactives to be used inside the turn.
   * these will have their locks be acquired dynamically see below for how that works.
   * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
   * (we do need to grab it, so it can be transferred to some other waiting transaction).
   * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
   * is executed, because the constructor typically accesses the dependencies to create its initial value.
   */
  override def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean)(f: => T): T = {
    dependencies.foreach(accessDynamic)
    val reactive = f
    val owner = reactive.lock.tryLock(this)
    assert(owner eq this, s"$this failed to acquire lock on newly created reactive $reactive")
    super.create(dependencies, dynamic)(reactive)
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def accessDynamic(dependency: Reactive): Unit = AcquireShared(dependency, this)
}

