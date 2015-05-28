package rescala.synchronization

import rescala.graph.Reactive

import scala.annotation.tailrec

object AcquireShared {

  def apply(reactive: Reactive, key: Key): Key = apply(reactive.lock, key)

  @tailrec
  def apply(lock: TurnLock, requester: Key): Key = {
    val oldOwner = lock.tryLock(requester)

    val res =
      if (oldOwner eq requester) Done(requester)
      else {
        Keychains.lockKeychains(requester, oldOwner) {
          // be aware that the owner of the lock could change at any time.
          // but it can not change when the owner is the requester or old owner,
          // because the keychain protects unlocking.ü
          lock.tryLock(requester) match {
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
      case Retry => apply(lock, requester)
      case Done(o) => o
    }
  }


  sealed trait Result[+R]
  object Await extends Result[Nothing]
  object Retry extends Result[Nothing]
  case class Done[R](r: R) extends Result[R]


}
