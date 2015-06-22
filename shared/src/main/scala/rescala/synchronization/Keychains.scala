package rescala.synchronization

import scala.annotation.tailrec

object Keychains {

  def locked[R](k1: Keychain, k2: Keychain)(f: => R): R = {
    val (first, second) = if (k1.id < k2.id) (k1, k2) else (k2, k1)
    first.synchronized { second.synchronized { f } }
  }

  @tailrec
  def lockKeychains[R](k1: Key, k2: Key)(f: => R): R = {
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
