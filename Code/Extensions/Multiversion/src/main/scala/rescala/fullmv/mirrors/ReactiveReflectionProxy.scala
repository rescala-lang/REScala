package rescala.fullmv.mirrors

import rescala.fullmv.FullMVTurn

trait ReactiveReflectionProxy[-P] {
  def asyncIncrementFrame(turn: FullMVTurn): Unit
  def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit
  def asyncResolvedUnchanged(turn: FullMVTurn): Unit
  def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit
  def asyncNewValue(turn: FullMVTurn, value: P): Unit
  def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit
}
