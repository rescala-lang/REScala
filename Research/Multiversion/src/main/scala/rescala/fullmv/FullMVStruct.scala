package rescala.fullmv

import rescala.core.Reactive
import rescala.core.{Reactive, Struct}

trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = FullMVState[P, FullMVTurn, Reactive[FullMVStruct], Reactive[FullMVStruct]]
}
