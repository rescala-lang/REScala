package rescala.synchronization

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec

object SyncUtil {

  /** locks the given locks in a global deterministic order */
  @tailrec
  def lockLanes[R](lo: Key*)(f: => R): R = {

    sealed trait LaneResult
    object Further extends LaneResult
    object Retry extends LaneResult
    case class Good(r: R) extends LaneResult


    val heads = lo.map(laneHead)

    heads.sortBy(_.id).foreach(_.keyLock.lock())
    (try {
      val afterHeads = heads.map(laneHead)
      if (afterHeads == heads) {
        if (heads zip lo forall (controls _).tupled) Good(f)
        else Retry
      }
      else Further
    }
    finally heads.foreach(_.keyLock.unlock())) match {
      case Further => lockLanes(heads: _*)(f)
      case Retry => lockLanes(lo: _*)(f)
      case Good(r) => r
    }
  }

  @tailrec
  def laneHead(k: Key): Key = k.prior match {
    case None => k
    case Some(p) => laneHead(p)
  }

  @tailrec
  def controls(pointer: Key, target: Key): Boolean =
    if (pointer == target) true
    else pointer.subsequent match {
      case None => false
      case Some(next) => controls(next, target)
    }


  val turnCounter = new AtomicInteger(0)
}
