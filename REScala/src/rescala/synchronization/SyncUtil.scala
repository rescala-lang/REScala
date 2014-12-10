package rescala.synchronization

import java.util.concurrent.locks.Lock

object SyncUtil {
  /** locks the given locks in a global deterministic order */
  def lockOrdered[R](lo: Lock*)(f: => R): R = {
    val sorted = lo.sortBy(System.identityHashCode)
    sorted.foreach(_.lock())
    try { f }
    finally sorted.foreach(_.unlock())
  }

}
