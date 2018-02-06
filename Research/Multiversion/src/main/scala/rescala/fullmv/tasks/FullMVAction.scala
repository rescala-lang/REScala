package rescala.fullmv.tasks

import java.util.concurrent.RecursiveAction

import rescala.fullmv.FullMVTurn

trait FullMVAction extends RecursiveAction {
  val turn: FullMVTurn
  override def compute(): Unit = {
    try { doCompute() } catch {
      case t: Throwable =>
        new Exception(this + " failed on " + Thread.currentThread().getName, t).printStackTrace()
    }
  }
  def doCompute(): Unit
}
