package rescala.synchronization

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec

object SyncUtil {

  /** locks the given locks in a global deterministic order */
  @tailrec
  def lockLanes[R](lo: Key*)(f: => R): R = {
    val heads = lo.map(laneHead)

    heads.sortBy(_.id).foreach(_.keyLock.lock())
    (try {
      val afterHeads = heads.map(laneHead)
      if (afterHeads == heads) Some(f)
      else None
    }
    finally heads.foreach(_.keyLock.unlock())) match {
      case None => lockLanes(heads: _*)(f)
      case Some(r) => r
    }
  }

  @tailrec
  def laneHead(k: Key): Key = k.prior match {
    case None => k
    case Some(p) => laneHead(p)
  }


  val counter = new AtomicInteger(0)

}
