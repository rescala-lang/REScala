package rescala.twoversion

import rescala.engine.Turn
import rescala.graph.{Reactive, Struct}

import scala.language.higherKinds


/**
  * Spore that implements both the buffered pulse and the buffering capabilities itself.
  *
  * @tparam P Pulse stored value type
  */
trait BufferedValueStruct[P, S <: Struct] extends ReadWriteValue[P, S] with Committable[S] {
  protected var current: P
  protected val transient: Boolean
  protected var owner: Token = null
  private var update: P = _

  override def write(value: P, token: Token): Boolean = {
    assert(owner == null || owner == token, s"buffer owned by $owner written by $token")
    update = value
    val res = owner == null
    owner = token
    res
  }
  override def get(token: Token): P = { if (token eq owner) update else current }
  override def base(token: Token): P = current

  override def commit(implicit turn: TwoVersionPropagation[S]): Unit = {
    if (!transient) current = update
    release(turn)
  }
  override def release(implicit turn: TwoVersionPropagation[S]): Unit = {
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
abstract class PropagationStructImpl[P, S <: Struct](override var current: P, override val transient: Boolean, initialIncoming: Set[Reactive[S]]) extends GraphStructType[S] with BufferedValueStruct[P, S] {
  protected var _incoming: Set[Reactive[S]] = initialIncoming
  protected var _outgoing: scala.collection.mutable.Map[Reactive[S], None.type] = rescala.util.WeakHashMap.empty


  def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = _incoming
  def updateIncoming(reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit = _incoming = reactives
  override def outgoing(implicit turn: Turn[S]): Iterator[Reactive[S]] = _outgoing.keysIterator
  override def discover(reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing.put(reactive, None)
  override def drop(reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing -= reactive
}

/**
  * Wrapper for a struct type combining GraphSpore and PulsingSpore
  */
trait GraphStruct extends Struct {
  override type State[P, S <: Struct] <: GraphStructType[S] with ReadWriteValue[P, S]
}

/**
  * Spore that can represent a node in a graph by providing information about incoming and outgoing edges.
  *
  * @tparam S Type of the reactive values that are connected to this struct
  */
trait GraphStructType[S <: Struct] {
  def incoming(implicit turn: Turn[S]): Set[Reactive[S]]
  def updateIncoming(reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit
  def outgoing(implicit turn: Turn[S]): Iterator[Reactive[S]]
  def discover(reactive: Reactive[S])(implicit turn: Turn[S]): Unit
  def drop(reactive: Reactive[S])(implicit turn: Turn[S]): Unit
}

case class Token(payload: AnyRef = null)


trait ReadValue[P] {
  def base(token: Token): P
  def get(token: Token): P
}

/**
  * Spore that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update
  *
  * @tparam P Pulse stored value type
  */
trait ReadWriteValue[P, S <: Struct] extends ReadValue[P] with Committable[S] {
  def write(value: P, token: Token): Boolean
}



