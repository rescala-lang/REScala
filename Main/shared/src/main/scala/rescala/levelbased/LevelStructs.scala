package rescala.levelbased

import rescala.graph.{ChangableGraphStruct, GraphStruct, PropagationSporeImpl, Pulse, PulseStruct}
import rescala.propagation.Turn
import scala.language.higherKinds

/**
  * Wrapper for a struct type that combines GraphSpore, PulsingSpore and is leveled
  */
trait LevelStruct extends ChangableGraphStruct {
  override type StructType[P, R] <: LevelStructType[R] with GraphStruct[R] with PulseStruct[P]
}

/**
  * Wrapper for the instance of LevelSpore
  */
trait SimpleStruct extends LevelStruct {
  override type StructType[P, R] = LevelStructTypeImpl[P, R]
}

/**
  * Graph struct that additionally can be assigned a level value that is used for topologically traversing the graph.
  *
  * @tparam R Type of the reactive values that are connected to this struct
  */
trait LevelStructType[R] extends GraphStruct[R] {
  def level(implicit turn: Turn[_]): Int
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int
}

/**
  * Implementation of a struct with graph and buffered pulse storage functionality that also support setting a level.
  *
  * @param current Pulse used as initial value for the struct
  * @param transient If a struct is marked as transient, changes to it can not be committed (and are released instead)
  * @param initialIncoming Initial incoming edges in the struct's graph
  * @tparam P Pulse stored value type
  * @tparam R Type of the reactive values that are connected to this struct
  */
class LevelStructTypeImpl[P, R](current: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends PropagationSporeImpl[P, R](current, transient, initialIncoming) with LevelStructType[R]  {
  var _level: Int = 0

  override def level(implicit turn: Turn[_]): Int = _level

  override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
}

