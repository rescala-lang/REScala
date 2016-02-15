package rescala.parrp

import rescala.graph.Reactive
import rescala.parrp.ParRP.{Await, Done, Retry}
import rescala.propagation.{FactoryReference, PropagationImpl}

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
  override def lockPhase(initialWrites: List[Reactive[TState]]): Unit = {
    val toVisit = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(toVisit.offer)

    while (!toVisit.isEmpty) {
      val reactive = toVisit.pop()
      if (!reactive.bud.lock.isOwner(key)) {
        if (reactive.bud.lock.tryLock(key) eq key)
          reactive.bud.outgoing.foreach {toVisit.offer}
        else {
          key.lockKeychain {
            key.releaseAll()
            key.keychain = new Keychain(key)
          }
          backoff.backoff()
          toVisit.clear()
          initialWrites.foreach(toVisit.offer)
        }
      }
    }
  }


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
        key.lockKeychain {
          assert(key.keychain == owner.keychain, "tried to transfer locks between keychains")
          key.keychain.addFallthrough(owner)
        }
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
        key.lockKeychain(key.keychain.removeFallthrough(owner))
        if (!sink.incoming(this).exists(_.bud.lock.isOwner(owner))) owner.turn.forget(sink)
      }
    }
    else super.drop(sink)(source)
  }

  def acquireShared(reactive: Reactive[TState]): Key = acquireShared(reactive.bud.lock, key)

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
