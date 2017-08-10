package rescala.fullmv.mirrors

import rescala.fullmv.FullMVTurn

trait ReactiveReflectionProxy[-P] {
  def incrementFrame(turn: FullMVTurn): Unit
  def incrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit
  def resolvedUnchanged(turn: FullMVTurn): Unit
  def resolvedUnchanged(turn: FullMVTurn, followFrame: FullMVTurn): Unit
  def newValue(turn: FullMVTurn, value: P): Unit
  def newValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit
}
