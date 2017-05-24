package rescala.fullmv

import rescala.fullmv.wsdeque.{NonBlockingCircularArrayWSDeque, WriteableQueue}
import rescala.graph.Reactive

import scala.annotation.tailrec

trait Task[+T, +R] {
  val txn: T
  val node: R
  def apply(queue: WriteableQueue[Task[T, R]]): Unit
}

class FullMVWorker(threadGroup: ThreadGroup, name: String, val ownQueue: NonBlockingCircularArrayWSDeque[Task[FullMVTurn, Reactive[FullMVStruct]]], val stealFrom: Array[NonBlockingCircularArrayWSDeque[Task[FullMVTurn, Reactive[FullMVStruct]]]]) extends Thread(threadGroup, name) {
  setDaemon(true)

  @tailrec private def stealRandom(i: Int): Task[FullMVTurn, Reactive[FullMVStruct]] = {
    if(i < stealFrom.length) {
      stealFrom(i).steal() match {
        case Some(task) => task
        case None =>
          stealRandom(i + 1)
      }
    } else {
      synchronized {
        wait(1)
      }
      stealRandom(0)
    }
  }

  final override def run(): Unit = {
    while("pigs" != "fly") {
      val task = ownQueue.popBottom().getOrElse(stealRandom(0))
      try {
        task(ownQueue)
      } catch {
        case t: Throwable =>
          new Exception(s"[${Thread.currentThread().getName}] task $task failed", t).printStackTrace()
      }
    }
  }
}
