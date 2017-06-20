package rescala.fullmv.tasks

import java.util.concurrent.RecursiveAction

import rescala.fullmv.FullMVTurn

trait FullMVAction extends RecursiveAction {
  val turn: FullMVTurn

  def doCompute(): Unit

  final override def compute(): Unit = try{
    doCompute()
  } catch {
    case e: Throwable =>
      new Exception(s"Task $this failed.", e).printStackTrace()
  }
}
