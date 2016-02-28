package rescala.locking

import scala.annotation.tailrec

object Keychains {

  def locked[R, InterTurn](k1: Keychain[InterTurn], k2: Keychain[InterTurn])(f: => R): R = {
    val (first, second) = if (k1.id < k2.id) (k1, k2) else (k2, k1)
    first.synchronized { second.synchronized { f } }
  }

  @tailrec
  def lockKeychains[R, InterTurn](k1: Key[InterTurn], k2: Key[InterTurn])(f: => R): R = {
    val kc1 = k1.keychain
    val kc2 = k2.keychain
    locked(kc1, kc2) {
      if (k1.keychain == kc1 && k2.keychain == kc2) Some(f)
      else None
    } match {
      case None => lockKeychains(k1, k2)(f)
      case Some(res) => res
    }
  }


}
