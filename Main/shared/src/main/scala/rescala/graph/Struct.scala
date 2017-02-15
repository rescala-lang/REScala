package rescala.graph

import rescala.propagation.{Committable, Turn}

import scala.language.{existentials, higherKinds, implicitConversions}

/**
  * Wrapper that adds a level of indirection for classes having a struct type dependency.
 */
trait Struct {
  /**
    * Spore type defined by this struct
    *
    * @tparam P Pulse stored value type
    * @tparam R Reactive value type represented by the struct
    */
  type StructType[P, R] <: PulseStruct[P] with ReadGraphStruct[R]
}

/**
  * Wrapper for a struct type combining GraphSpore and PulsingSpore
  */
trait ChangableGraphStruct extends Struct {
  override type StructType[P, R] <: GraphStruct[R] with PulseStruct[P]
}

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
  * Spore that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update
  *
  * @tparam P Pulse stored value type
  */
trait PulseStruct[P] {
  def set(value: Pulse[P])(implicit turn: Turn[_]): Unit
  def base(implicit turn: Turn[_]): Pulse[P]
  def get(implicit turn: Turn[_]): Pulse[P]
}

/**
  * Spore that implements both the buffered pulse and the buffering capabilities itself.
  *
  * @tparam P Pulse stored value type
  */
trait BufferedSpore[P] extends PulseStruct[P] with Committable {
  protected var current: Pulse[P]
  protected val transient: Boolean
  protected var owner: Turn[_] = null
  private var update: Pulse[P] = Pulse.NoChange

  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    update = value
    if (owner == null) turn.schedule(this)
    owner = turn
  }
  override def get(implicit turn: Turn[_]): Pulse[P] = { if (turn eq owner) update else current }
  override def base(implicit turn: Turn[_]): Pulse[P] = current

  override def commit(implicit turn: Turn[_]): Unit = {
    if (!transient) current = update
    release(turn)
  }
  override def release(implicit turn: Turn[_]): Unit = {
    update = Pulse.NoChange
    owner = null
  }
}

trait ReadGraphStruct[R] {
  def incoming(implicit turn: Turn[_]): Set[R]
}

/**
  * Spore that can represent a node in a graph by providing information about incoming and outgoing edges.
  *
  * @tparam R Type of the reactive values that are connected to this struct
  */
trait GraphStruct[R] extends ReadGraphStruct[R] {
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit
  def outgoing(implicit turn: Turn[_]): Iterator[R]
  def discover(reactive: R)(implicit turn: Turn[_]): Unit
  def drop(reactive: R)(implicit turn: Turn[_]): Unit
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
  * Implementation of a struct with graph functionality and a buffered pulse storage.
  *
  * @param current Pulse used as initial value for the struct
  * @param transient If a struct is marked as transient, changes to it can not be committed (and are released instead)
  * @param initialIncoming Initial incoming edges in the struct's graph
  * @tparam P Pulse stored value type
  * @tparam R Type of the reactive values that are connected to this struct
  */
abstract class PropagationSporeImpl[P, R](override var current: Pulse[P], override val transient: Boolean, initialIncoming: Set[R]) extends GraphStruct[R] with BufferedSpore[P] {
  private var _incoming: Set[R] = initialIncoming
  private var _outgoing: scala.collection.mutable.Map[R, Boolean] = rescala.util.WeakHashMap.empty


  def incoming(implicit turn: Turn[_]): Set[R] = _incoming
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives
  override def outgoing(implicit turn: Turn[_]): Iterator[R] = _outgoing.keysIterator
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.put(reactive, true)
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing -= reactive
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
