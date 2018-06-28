package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.Struct
import rescala.twoversion._

import scala.language.higherKinds

/**
  * Wrapper for a struct type that combines GraphSpore, PulsingSpore and is leveled
  */
trait LevelStruct extends TwoVersionStruct {
  override type State[P, S <: Struct] <: LevelState[S] with GraphState[S] with ReadWriteValue[P, S]
}

/**
  * Wrapper for the instance of LevelSpore
  */
trait SimpleStruct extends LevelStruct {
  override type State[P, S <: Struct] = LevelStateImpl[P, S]
}

/**
  * Graph struct that additionally can be assigned a level value that is used for topologically traversing the graph.
  *
  * @tparam S Type of the reactive values that are connected to this struct
  */
trait LevelState[S <: Struct] extends GraphState[S] {
  def level(): Int
  def updateLevel(i: Int): Int
}

/** Implementation of a struct with graph and buffered pulse storage functionality that also support setting a level. */
class LevelStateImpl[P, S <: Struct](ip: InitValues[P])
  extends PropagationStructImpl[P, S](ip) with LevelState[S] {

  var _level: Int = 0

  override def level(): Int = _level

  override def updateLevel(i: Int): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
}

