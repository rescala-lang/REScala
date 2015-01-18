package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.LevelQueue
import rescala.turns.Turn

object Keychains {

  sealed trait Result[+R]
  object Await extends Result[Nothing]
  object Retry extends Result[Nothing]
  case class Done[R](r: R) extends Result[R]

  def locked[R](k1: Keychain, k2: Keychain)(f: => R): R = {
    val (first, second) = if (k1.id < k2.id) (k1, k2) else (k2, k1)
    first.synchronized {
      second.synchronized {
        f
      }
    }
  }

  def lockKeys[R](k1: Key, k2: Key)(f: => R): R = {
    val kc1 = k1.keychain
    val kc2 = k2.keychain
    locked(kc1, kc2) {
      if (k1.keychain == kc1 && k2.keychain == kc2) Some(f)
      else None
    } match {
      case None => lockKeys(k1, k2)(f)
      case Some(res) => res
    }
  }

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  def lockReachable(initial: List[Reactive], acquire: Reactive => Boolean)(implicit turn: Turn): Unit = {
    val lq = new LevelQueue()
    initial.foreach(lq.enqueue(-42))

    lq.evaluateQueue { reactive =>
      if (acquire(reactive))
        reactive.outgoing.get.foreach(lq.enqueue(-42))
      else {
        lq.clear()
        initial.foreach(lq.enqueue(-42))
      }
    }
  }

}
