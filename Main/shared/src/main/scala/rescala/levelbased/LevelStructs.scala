package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.Struct
import rescala.twoversion._

import scala.language.higherKinds


trait LevelStruct extends TwoVersionStruct {
  override type State[P, S <: Struct] <: LevelState[P, S] with GraphState[S] with ReadWriteValue[P, S]
}


trait LevelStructImpl extends LevelStruct {
  override type State[P, S <: Struct] = LevelState[P, S]
}


class LevelState[P, S <: Struct](ip: InitValues[P])
  extends GraphStateImpl[P, S](ip) {

  private var _level: Int = 0

  def level(): Int = _level

  def updateLevel(i: Int): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
}

