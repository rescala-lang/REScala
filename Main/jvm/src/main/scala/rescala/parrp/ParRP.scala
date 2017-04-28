package rescala.parrp

import rescala.engine.ValuePersistency
import rescala.graph.{Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStructTypeImpl}
import rescala.locking._

trait ParRPInterTurn {
  private type TState = ParRP

  def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit
  def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit

  def forget(reactive: Reactive[TState]): Unit
  def admit(reactive: Reactive[TState]): Unit

}

class ParRPStructType[P, S <: Struct](current: P, transient: Boolean, val lock: TurnLock[ParRPInterTurn])
  extends LevelStructTypeImpl[P, S](current, transient)


class ParRP(backoff: Backoff, priorTurn: Option[ParRP]) extends LevelBasedPropagation[ParRP] with ParRPInterTurn with LevelStruct {
  override type State[P, S <: Struct] = ParRPStructType[P, S]

  private type TState = ParRP



  override def writeState[P](pulsing: Reactive[TState])(value: pulsing.Value): Unit = {
    assert({
      val wlo: Option[Key[ParRPInterTurn]] = Option(pulsing.state.lock.getOwner)
      wlo.fold(true)(_ eq key)},
      s"buffer ${pulsing.state}, controlled by ${pulsing.state.lock} with owner ${pulsing.state.lock.getOwner}" +
        s" was written by $this who locks with ${key}, by now the owner is ${pulsing.state.lock.getOwner}")
    super.writeState(pulsing)(value)
  }


  override def toString: String = s"ParRP(${key.id})"

  final val key: Key[ParRPInterTurn] = new Key(this)

  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): ParRPStructType[P, ParRP] = {
    val lock = new TurnLock[ParRPInterTurn]
    val owner = lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive")
    new ParRPStructType(valuePersistency.initialValue, valuePersistency.isTransient, lock)
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = key.releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)


  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: Traversable[Reactive[TState]]): Unit = {
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
      owner.turn.discover(sink)(source)
      if (source.state.lock.isWriteLock) {
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
      if (source.state.lock.isWriteLock) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.state.incoming(this).exists{ inc =>
          val lock = inc.state.lock
          lock.isOwner(owner) && lock.isWriteLock
        }) owner.turn.forget(sink)
      }
    }
    else super.drop(sink)(source)
  }

  def acquireShared(reactive: Reactive[TState]): Key[ParRPInterTurn] = Keychains.acquireShared(reactive.state.lock, key)
}

