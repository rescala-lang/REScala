package rescala.fullmv

import rescala.core.{Reactive, Struct}


trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]]
}
