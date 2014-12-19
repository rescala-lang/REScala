package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.LevelQueue
import rescala.turns.Turn

import scala.annotation.tailrec

object SyncUtil {
  
  sealed trait Result[+R]
  object Await extends Result[Nothing]
  object Retry extends Result[Nothing]
  case class Done[R](r: R) extends Result[R]
  

  @tailrec
  def lockLanes[R](mine: Key, originalTarget: Key)(f: Key => R): R = {

    val targetHead = laneHead(originalTarget)

    val (first, second) = if (mine.id < targetHead.id) (mine, targetHead) else (targetHead, mine)

    first.synchronized {
      second.synchronized {
        if (laneHead(targetHead) == targetHead) {
          if (targetHead.controls(originalTarget)) Done(f(targetHead))
          else Retry
        }
        else Await
      }
    } match {
      case Await => lockLanes(mine, targetHead)(f)
      case Retry => lockLanes(mine, originalTarget)(f)
      case Done(r) => r
    }
  }

  @tailrec
  def laneHead(k: Key): Key = k.prior match {
    case None => k
    case Some(p) => laneHead(p)
  }

  def wantReachable(key: Key, reactive: Reactive)(implicit turn: Turn): Unit = {
    SyncUtil.lockReachable(reactive :: Nil, {r => r.lock.wantedBy(key); true})
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
