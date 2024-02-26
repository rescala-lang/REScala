package reactives.locking

import scala.annotation.tailrec

object Keychains {

  private def lockKeychains[R, InterTurn](
      k1: Key[InterTurn],
      k2: Key[InterTurn]
  )(f: (Keychain[InterTurn], Keychain[InterTurn]) => R): R = {
    while (true) {
      val kc1             = k1.keychain
      val kc2             = k2.keychain
      val (first, second) = if (kc1.id < kc2.id) (kc1, kc2) else (kc2, kc1)
      first.synchronized {
        second.synchronized {
          if (k1.keychain == kc1 && k2.keychain == kc2) return f(kc1, kc2)
        }
      }
    }
    throw new AssertionError("broke out of infinite loop")
  }

  private sealed trait Result[+R]
  private object Await             extends Result[Nothing]
  private object Retry             extends Result[Nothing]
  private case class Done[R](r: R) extends Result[R]

  @tailrec
  def acquireShared[ParRPInterTurn](
      lock: ReLock[ParRPInterTurn],
      requester: Key[ParRPInterTurn]
  ): Key[ParRPInterTurn] = {
    val oldOwner = lock.tryLock(requester, write = false)

    val res: Result[Key[ParRPInterTurn]] =
      if (oldOwner eq requester) Done(requester)
      else {
        Keychains.lockKeychains(requester, oldOwner) { (kcRequester, kcOwner) =>
          // be aware that the owner of the lock could change at any time.
          // but it can not change when the owner is the requester or old owner,
          // because the keychain protects unlocking.
          lock.tryLock(requester, write = false) match {
            // make sure the other owner did not unlock before we got his master lock
            case owner if owner eq requester     => Done(requester)
            case owner if owner ne oldOwner      => Retry
            case owner if kcRequester eq kcOwner => Done(owner)
            case owner => // owner here must be equal to the oldOwner, whose keychain is locked
              lock.share(requester)
              kcOwner.append(kcRequester)
              Await
          }
        }
      }
    res match {
      case Await =>
        requester.await()
        lock.acquired(requester)
        requester
      case Retry   => acquireShared(lock, requester)
      case Done(o) => o
    }
  }

}
