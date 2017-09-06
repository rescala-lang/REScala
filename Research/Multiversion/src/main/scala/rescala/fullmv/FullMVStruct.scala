package rescala.fullmv

import rescala.core.Node.InDep
import rescala.core.{Reactive, Struct}

trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = FullMVState[P, FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]]
}
