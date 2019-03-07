package rescala.fullmv

import rescala.core.{ReSource, Derived, Struct}

trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = FullMVState[P, FullMVTurn, ReSource[FullMVStruct], Derived[FullMVStruct]]
}
