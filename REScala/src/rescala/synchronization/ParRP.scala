package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.PropagationImpl
import rescala.turns.Engines

class ParRP(var backOff: Int) extends EngineReference[ParRP](Engines.spinning) with PropagationImpl {

  override def toString: String = s"Prelock(${ key.id })"

  final val key: Key = new Key(this)

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
    val owner = reactive.lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive $reactive")
    super.create(dependencies, dynamic)(reactive)
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def realeasePhase(): Unit = key.releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def accessDynamic(dependency: Reactive): Unit = AcquireShared(dependency, key)


  var currentBackOff = backOff

  override def lockPhase(initialWrites: List[Reactive]): Unit = Keychains.lockReachable(initialWrites, acquireWrite)

  def acquireWrite(reactive: Reactive): Boolean =
    if (reactive.lock.tryLock(key) eq key) true
    else {
      key.lockKeychain {
        key.releaseAll()
        key.keychain = new Keychain(key)
      }
      if (currentBackOff == 0) {
        AcquireShared(reactive, key)
        backOff /= 2
        currentBackOff = backOff
      }
      else if (currentBackOff > 0) {
        currentBackOff -= 1
      }
      false
    }


  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correcly passing the locks is moved to the commit phase */
  override def register(sink: Reactive)(source: Reactive): Unit = {
    val owner = AcquireShared(source, key)
    if (owner ne key) {
      if (!source.outgoing.get.contains(sink)) {
        owner.turn.register(sink)(source)
        owner.turn.admit(sink)
        key.lockKeychain(key.keychain.addFallthrough(owner))
      }
    }
    else {
      super.register(sink)(source)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def unregister(sink: Reactive)(source: Reactive): Unit = {
    val owner = AcquireShared(source, key)
    if (owner ne key) {
      owner.turn.unregister(sink)(source)
      key.lockKeychain(key.keychain.removeFallthrough(owner))
      if (!sink.incoming(this).exists(_.lock.isOwner(owner))) owner.turn.forget(sink)
    }
    else super.unregister(sink)(source)
  }

}