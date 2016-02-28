package rescala.parrp

import rescala.graph.Reactive
import rescala.locking._
import rescala.propagation.LevelBasedPropagation

trait ParRPInterTurn {
  private type TState = ParRPStruct.type

  def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit
  def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit

  def forget(reactive: Reactive[TState]): Unit
  def admit(reactive: Reactive[TState]): Unit

}

class ParRP(backoff: Backoff) extends LevelBasedPropagation[ParRPStruct.type] with ParRPInterTurn {

  private type TState = ParRPStruct.type

  override def toString: String = s"ParRP(${key.id})"

  /** used to create state containers of each reactive */
  override def bufferFactory: TState = ParRPStruct

  final val key: Key[ParRPInterTurn] = new Key(this)

  /**
    * creating a signal causes some unpredictable reactives to be used inside the turn.
    * these will have their locks be acquired dynamically see below for how that works.
    * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
    * (we do need to grab it, so it can be transferred to some other waiting transaction).
    * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
    * is executed, because the constructor typically accesses the dependencies to create its initial value.
    */
  override def create[T <: Reactive[TState]](dependencies: Set[Reactive[TState]], dynamic: Boolean)(f: => T): T = {
    dependencies.foreach(dependencyInteraction)
    val reactive = f
    val owner = reactive.bud.lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive $reactive")
    super.create(dependencies, dynamic)(reactive)
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = key.releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def dependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)


  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: List[Reactive[TState]]): Unit = {
    val toVisit = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(toVisit.offer)

    while (!toVisit.isEmpty) {
      val reactive = toVisit.pop()
      if (!reactive.bud.lock.isOwner(key)) {
        if (reactive.bud.lock.tryLock(key) eq key)
          reactive.bud.outgoing.foreach {toVisit.offer}
        else {
          key.reset()
          backoff.backoff()
          toVisit.clear()
          initialWrites.foreach(toVisit.offer)
        }
      }
    }
    super.preparationPhase(initialWrites)
  }


  override def forget(reactive: Reactive[TState]): Unit = levelQueue.remove(reactive)
  override def admit(reactive: Reactive[TState]): Unit = levelQueue.enqueue(reactive.bud.level)(reactive)

  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correctly passing the locks is moved to the commit phase */
  override def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit = {

    val owner = acquireShared(source)
    if (owner ne key) {
      if (!source.bud.lock.isWriteLock) {
        owner.turn.discover(sink)(source)
      }
      else if (!source.bud.outgoing.contains(sink)) {
        owner.turn.discover(sink)(source)
        owner.turn.admit(sink)
        key.lockKeychain { _.addFallthrough(owner) }
      }
    }
    else {
      super.discover(sink)(source)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit = {


    val owner = acquireShared(source)
    if (owner ne key) {
      owner.turn.drop(sink)(source)
      if (!source.bud.lock.isWriteLock) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.bud.incoming(this).exists(_.bud.lock.isOwner(owner))) owner.turn.forget(sink)
      }
    }
    else super.drop(sink)(source)
  }

  def acquireShared(reactive: Reactive[TState]): Key[ParRPInterTurn] = Keychains.acquireShared(reactive.bud.lock, key)


}

