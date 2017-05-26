package rescala.fullmv.tasks

import java.util.concurrent.RecursiveAction

import rescala.fullmv.FullMVTurn

trait FullMVAction extends RecursiveAction {
  val turn: FullMVTurn
}
