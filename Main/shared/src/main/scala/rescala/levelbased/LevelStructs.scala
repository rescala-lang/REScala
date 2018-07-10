package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.Struct
import rescala.twoversion._

import scala.language.higherKinds


trait LevelStruct extends TwoVersionStruct {
  override type State[P, S <: Struct] <: LevelState[S] with GraphState[S] with ReadWriteValue[P, S]
}


trait LevelStructImpl extends LevelStruct {
  override type State[P, S <: Struct] = LevelStateImpl[P, S]
}

/** State with an additional level in the graph. */
trait LevelState[S <: Struct] extends GraphState[S] {
  def level(): Int
  def updateLevel(i: Int): Int
}

class LevelStateImpl[P, S <: Struct](ip: InitValues[P])
  extends GraphStateImpl[P, S](ip) with LevelState[S] {

  var _level: Int = 0

  override def level(): Int = _level

  override def updateLevel(i: Int): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
}

