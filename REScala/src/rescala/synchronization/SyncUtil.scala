package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.LevelQueue
import rescala.turns.Turn

import scala.annotation.tailrec

object SyncUtil {

  @tailrec
  def lockLanes[R](mine: Key, originalTarget: Key)(f: => R): R = {

    sealed trait LaneResult
    object Further extends LaneResult
    object Retry extends LaneResult
    case class Good(r: R) extends LaneResult


    val targetHead = laneHead(originalTarget)

    val (first, second) = if (mine.id < targetHead.id) (mine, targetHead) else (targetHead, mine)

    first.synchronized {
      second.synchronized {
        if (laneHead(targetHead) == targetHead) {
          if (controls(targetHead, originalTarget)) Good(f)
          else Retry
        }
        else Further
      }
    } match {
      case Further => lockLanes(mine, targetHead)(f)
      case Retry => lockLanes(mine, originalTarget)(f)
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

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  def lockReachable(initial: List[Reactive], acquire: Reactive => Boolean)(implicit turn: Turn): Unit = {
    val lq = new LevelQueue()
    initial.foreach(lq.enqueue(-42))

    lq.evaluateQueue { reactive =>
      if (acquire(reactive))
        reactive.dependants.get.foreach(lq.enqueue(-42))
      else {
        lq.clear()
        initial.foreach(lq.enqueue(-42))
      }
    }
  }

}
