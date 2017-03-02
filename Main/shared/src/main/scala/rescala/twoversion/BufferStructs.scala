package rescala.twoversion

import rescala.graph.{GraphStruct, Pulse, ReadWritePulseStruct}
import rescala.propagation.Turn


/**
  * Spore that implements both the buffered pulse and the buffering capabilities itself.
  *
  * @tparam P Pulse stored value type
  */
trait BufferedPulseStruct[P] extends ReadWritePulseStruct[P] with Committable {
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

/**
  * Implementation of a struct with graph functionality and a buffered pulse storage.
  *
  * @param current Pulse used as initial value for the struct
  * @param transient If a struct is marked as transient, changes to it can not be committed (and are released instead)
  * @param initialIncoming Initial incoming edges in the struct's graph
  * @tparam P Pulse stored value type
  * @tparam R Type of the reactive values that are connected to this struct
  */
abstract class PropagationStructImpl[P, R](override var current: Pulse[P], override val transient: Boolean, initialIncoming: Set[R]) extends GraphStruct[R] with BufferedPulseStruct[P] {
  private var _incoming: Set[R] = initialIncoming
  private var _outgoing: scala.collection.mutable.Map[R, Boolean] = rescala.util.WeakHashMap.empty


  def incoming(implicit turn: Turn[_]): Set[R] = _incoming
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives
  override def outgoing(implicit turn: Turn[_]): Iterator[R] = _outgoing.keysIterator
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.put(reactive, true)
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing -= reactive
}
