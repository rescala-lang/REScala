package rescala.scheduler.levelbased

import rescala.core.Struct
import rescala.scheduler.twoversion._

trait LevelStruct extends TwoVersionStruct {
  override type State[V, S <: Struct] <: LevelState[V, S]
}

trait LevelStructImpl extends LevelStruct {
  override type State[V, S <: Struct] = LevelState[V, S]
}

class LevelState[V, S <: Struct](ip: V) extends TwoVersionState[V, S](ip) {

  private var _level: Int = 0

  def level(): Int = _level

  def updateLevel(i: Int): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
}
