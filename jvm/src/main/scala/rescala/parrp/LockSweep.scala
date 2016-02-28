package rescala.parrp

import java.util

import rescala.graph._
import rescala.locking._
import rescala.propagation.{CommonPropagationImpl, Turn}

object LSStruct extends PropagationStruct {
  override type Spore[R] = LSSporeP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] = {
    val lock = new TurnLock[LSInterTurn]
    new LSSporeP[P, R](initialValue, transient, lock, initialIncoming)
  }


}


class LSSporeP[P, R](current: Pulse[P], transient: Boolean, val lock: TurnLock[LSInterTurn], initialIncoming: Set[R])
  extends PropagationSporeImpl[P, R](current, transient, initialIncoming) {

  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    assert(turn match {
      case pessimistic: LockSweep =>
        val wlo: Option[Key[LSInterTurn]] = Option(lock).map(_.getOwner)
        assert(wlo.fold(true)(_ eq pessimistic.key),
          s"buffer owned by $owner, controlled by $lock with owner ${wlo.get}" +
            s" was written by $turn who locks with ${pessimistic.key}, by now the owner is ${lock.getOwner}")
        true
      case _ =>
        throw new IllegalStateException(s"locksweep buffer used with wrong turn")
    })
    super.set(value)
  }
}

trait LSInterTurn {

}

class LockSweep(backoff: Backoff) extends CommonPropagationImpl[LSStruct.type] with LSInterTurn {

  private type TState = LSStruct.type

  val sorted = new util.ArrayList[Reactive[TState]]

  final val key: Key[LSInterTurn] = new Key(this)

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: List[Reactive[TState]]): Unit = {
    val stack = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(stack.push)

    while (!stack.isEmpty) {
      val reactive = stack.pop()
      if (reactive.bud.lock.isOwner(key)) {
        sorted.add(reactive)
      }
      else {
        if (reactive.bud.lock.tryLock(key) eq key) {
          // we add the reactive again, so we can enter it into the `sorted` when the childern are processed
          stack.push(reactive)
          reactive.bud.outgoing(this).foreach { r =>
            if (!r.bud.lock.isOwner(key)) stack.push(r)
          }

        }
        else {
          key.reset()
          backoff.backoff()
          stack.clear()
          sorted.clear()
          initialWrites.foreach(stack.push)
        }
      }
    }
  }

  override def propagationPhase(): Unit = {
    val it = sorted.listIterator(sorted.size())
    while (it.hasPrevious) it.previous().reevaluate()(this)
  }



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

    if (dynamic) {
      throw new IllegalStateException("dynamic reactives are currently unsupported")
    }
    else {
      dependencies.foreach(discover(reactive))
    }
    reactive
  }

  def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit = source.bud.discover(sink)(this)

  def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit = source.bud.drop(sink)(this)


  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = key.releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def dependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)

  def acquireShared(reactive: Reactive[TState]): Key[LSInterTurn] = Keychains.acquireShared(reactive.bud.lock, key)


  override def bufferFactory: TState = LSStruct
}
