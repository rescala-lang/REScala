package rescala.parrp

import rescala.graph.{Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct}
import rescala.locking._

trait ParRPInterTurn {
  private type TState = ParRP

  def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit
  def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit

  def forget(reactive: Reactive[TState]): Unit
  def admit(reactive: Reactive[TState]): Unit

}

class ParRP(backoff: Backoff, priorTurn: Option[ParRP]) extends LevelBasedPropagation[ParRP] with ParRPInterTurn with LevelStruct {
  override type State[P, S <: Struct] = ParRPStructType[P, S]

  private type TState = ParRP


  override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[TState]], hasState: Boolean): TState#State[P, ParRP] = {
    val lock = new TurnLock[ParRPInterTurn]
    new ParRPStructType[P, ParRP](initialValue, transient, lock, initialIncoming)
  }


  override def toString: String = s"ParRP(${key.id})"

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
    dependencies.foreach(dynamicDependencyInteraction)
    val reactive = f
    val owner = reactive.state.lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive $reactive")
    super.create(dependencies, dynamic)(reactive)
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = key.releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)


  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: Traversable[Reactive[TState]]): Unit = {
    implicit val ticket = makeTicket()
    val toVisit = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(toVisit.offer)
    val priorKey = priorTurn.map(_.key).orNull

    while (!toVisit.isEmpty) {
      val reactive = toVisit.pop()
      val owner = reactive.state.lock.getOwner
      if ((priorKey ne null) && (owner eq priorKey)) throw new IllegalStateException(s"$this tried to lock reactive $reactive owned by its parent $priorKey")
      if (owner ne key) {
        if (reactive.state.lock.tryLock(key) eq key)
          reactive.state.outgoing(this).foreach {toVisit.offer}
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
  override def admit(reactive: Reactive[TState]): Unit = levelQueue.enqueue(reactive.state.level(this))(reactive)

  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correctly passing the locks is moved to the commit phase */
  override def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit = {

    val owner = acquireShared(source)
    if (owner ne key) {
      if (!source.state.lock.isWriteLock) {
        owner.turn.discover(sink)(source)
      }
      else if (!source.state.outgoing(this).contains(sink)) {
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
      if (!source.state.lock.isWriteLock) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.state.incoming(this).exists(_.state.lock.isOwner(owner))) owner.turn.forget(sink)
      }
    }
    else super.drop(sink)(source)
  }

  def acquireShared(reactive: Reactive[TState]): Key[ParRPInterTurn] = Keychains.acquireShared(reactive.state.lock, key)
}

