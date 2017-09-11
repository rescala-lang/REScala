package rescala.fullmv.tasks

import java.util.concurrent.RecursiveAction

import rescala.fullmv.FullMVTurn

import scala.annotation.tailrec

trait FullMVAction extends RecursiveAction {
  val turn: FullMVTurn

  def doCompute(): Traversable[FullMVAction]

  @tailrec final override def compute(): Unit = {
    val forks = try{
     doCompute()
    } catch {
      case e: Throwable =>
        new Exception(s"Task $this failed on Thread ${Thread.currentThread().getName}.", e).printStackTrace()
        Traversable.empty
    }
    val self = forks.head
    for(fork <- forks.tail) fork.fork()
    self.compute()
  }
}
