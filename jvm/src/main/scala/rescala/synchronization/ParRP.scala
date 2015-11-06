package rescala.synchronization

import rescala.graph.{ParRPSpores, Pulsing, Reactive}
import rescala.propagation.{LevelQueue, PropagationImpl}
import rescala.synchronization.ParRP.{Await, Done, Retry}

import scala.annotation.tailrec

class ParRP(backoff: Backoff) extends FactoryReference[ParRPSpores.type](ParRPSpores) with PropagationImpl[ParRPSpores.type] {

  type TState = ParRPSpores.type

  override def toString: String = s"ParRP(${key.id})"

  final val key: Key = new Key(this)

  /**
   * creating a signal causes some unpredictable reactives to be used inside the turn.
   * these will have their locks be acquired dynamically see below for how that works.
   * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
   * (we do need to grab it, so it can be transferred to some other waiting transaction).
   * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
   * is executed, because the constructor typically accesses the dependencies to create its initial value.
   */
  override def create[T <: Reactive[TState]](dependencies: Set[Reactive[TState]], dynamic: Boolean)(f: => T): T = {
    dependencies.foreach(accessDynamic)
    val reactive = f
    val owner = reactive.lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive $reactive")
    super.create(dependencies, dynamic)(reactive)
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = key.releaseAll()

  /** allow turn to handle dynamic access to reactives */
  override def accessDynamic(dependency: Reactive[TState]): Unit = acquireShared(dependency)


  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def lockPhase(initialWrites: List[Reactive[TState]]): Unit = {
    val lq = new LevelQueue()
    initialWrites.foreach(lq.enqueue(-42))

    lq.evaluateQueue { reactive =>
      if (reactive.lock.tryLock(key) eq key) reactive.outgoing.get.foreach(lq.enqueue(-42))
      else {
        key.lockKeychain {
          key.releaseAll()
          key.keychain = new Keychain(key)
        }
        backoff.backoff()
        lq.clear()
        initialWrites.foreach(lq.enqueue(-42))
      }
    }
  }


  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correctly passing the locks is moved to the commit phase */
  override def register(sink: Reactive[TState])(source: Reactive[TState]): Unit = {
    val owner = acquireShared(source)
    if (owner ne key) {
      if (!source.lock.isWriteLock) {
        owner.turn.register(sink)(source)
      }
      else if (!source.outgoing.get.contains(sink)) {
        owner.turn.register(sink)(source)
        owner.turn.admit(sink)
        key.lockKeychain {
          assert(key.keychain == owner.keychain, "tried to transfer locks between keychains")
          key.keychain.addFallthrough(owner)
        }
      }
    }
    else {
      super.register(sink)(source)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def unregister(sink: Reactive[TState])(source: Reactive[TState]): Unit = {
    val owner = acquireShared(source)
    if (owner ne key) {
      owner.turn.unregister(sink)(source)
      if (!source.lock.isWriteLock) {
        key.lockKeychain(key.keychain.removeFallthrough(owner))
        if (!sink.incoming(this).exists(_.lock.isOwner(owner))) owner.turn.forget(sink)
      }
    }
    else super.unregister(sink)(source)
  }

  def acquireShared(reactive: Reactive[TState]): Key = acquireShared(reactive.lock, key)

  @tailrec
  private def acquireShared(lock: TurnLock, requester: Key): Key = {
    val oldOwner = lock.tryLock(requester, write = false)

    val res =
      if (oldOwner eq requester) Done(requester)
      else {
        Keychains.lockKeychains(requester, oldOwner) {
          // be aware that the owner of the lock could change at any time.
          // but it can not change when the owner is the requester or old owner,
          // because the keychain protects unlocking.
          lock.tryLock(requester, write = false) match {
            // make sure the other owner did not unlock before we got his master lock
            case owner if owner eq requester => Done(requester)
            case owner if owner ne oldOwner => Retry
            case owner if requester.keychain eq owner.keychain => Done(owner)
            case owner => // owner here must be equal to the oldOwner, whose keychain is locked
              lock.share(requester)
              owner.keychain.append(requester.keychain)
              Await
          }
        }
      }
    res match {
      case Await =>
        requester.await()
        lock.acquired(requester)
        requester
      case Retry => acquireShared(lock, requester)
      case Done(o) => o
    }
  }
}

private object ParRP {

  sealed trait Result[+R]
  object Await extends Result[Nothing]
  object Retry extends Result[Nothing]
  case class Done[R](r: R) extends Result[R]

}
