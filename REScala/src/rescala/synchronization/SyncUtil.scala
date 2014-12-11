package rescala.synchronization

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

object SyncUtil {

  /** locks the given locks in a global deterministic order */
  def lockOrdered[R](lo: Key*)(f: => R): R = {
    val sorted = lo.sortBy(_.id)
    sorted.foreach(_.keyLock.lock())
    try { f }
    finally sorted.foreach(_.keyLock.unlock())
  }


  val counter = new AtomicInteger(0)

}
