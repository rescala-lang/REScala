package rescala.twoversion

import rescala.core.Initializer.InitValues
import rescala.core.{ReSource, Reactive, Struct}

import scala.language.higherKinds

trait TwoVersionStruct extends GraphStruct {
  override type State[P, S <: Struct, N] <: GraphStructType[S] with ReadWriteValue[P, S, N]
}

/**
  * Spore that implements both the buffered pulse and the buffering capabilities itself.
  *
  * @tparam P Pulse stored value type
  */
class BufferedValueStruct[P, S <: Struct, N](ip: InitValues[P, N]) extends ReadWriteValue[P, S, N] with Committable[S] {
  var current: P = ip.initialValue
  var notification: N = ip.initialNotification
  protected var owner: Token = null
  private var update: P = _

  override def write(value: P, token: Token): Boolean = {
    assert(owner == null || owner == token, s"buffer owned by $owner written by $token")
    update = value
    val res = owner == null
    owner = token
    res
  }
  override def setNotificaiton(notification: N, token: Token): Boolean = {
    assert(owner == null || owner == token, s"buffer owned by $owner written by $token")
    this.notification = notification
    val res = owner == null
    owner = token
    res
  }
  override def base(token: Token): P = current
  override def get(token: Token): P = {if (token eq owner) update else current}
  override def notification(token: Token): N = {if (token eq owner) notification else ip.initialNotification}

  override def commit(turn: TwoVersionPropagation[S]): Unit = {
    current = update
    release(turn)
  }
  override def release(turn: TwoVersionPropagation[S]): Unit = {
    notification = null.asInstanceOf[N]
    owner = null
  }
}

/**
  * Implementation of a struct with graph functionality and a buffered pulse storage.
  *
  * @param current Pulse used as initial value for the struct
  * @tparam P Pulse stored value type
  * @tparam S Type of the reactive values that are connected to this struct
  */
abstract class PropagationStructImpl[P, S <: Struct, N](ip: InitValues[P, N]) extends BufferedValueStruct[P, S, N](ip) with GraphStructType[S] {
  protected var _incoming: Set[ReSource[S]] = Set.empty
  protected var _outgoing: scala.collection.mutable.Map[Reactive[S], None.type] = rescala.util.WeakHashMap.empty


  def incoming(): Set[ReSource[S]] = _incoming
  def updateIncoming(reactives: Set[ReSource[S]]): Unit = _incoming = reactives
  override def outgoing(): Iterable[Reactive[S]] = _outgoing.keys
  override def discover(reactive: Reactive[S]): Unit = _outgoing.put(reactive, None)
  override def drop(reactive: Reactive[S]): Unit = _outgoing -= reactive
}

/**
  * Wrapper for a struct type combining GraphSpore and PulsingSpore
  */
trait GraphStruct extends Struct {
  override type State[P, S <: Struct, N] <: GraphStructType[S] with ReadWriteValue[P, S, N]
}

/**
  * Spore that can represent a node in a graph by providing information about incoming and outgoing edges.
  *
  * @tparam S Type of the reactive values that are connected to this struct
  */
trait GraphStructType[S <: Struct] {
  def incoming(): Set[ReSource[S]]
  def updateIncoming(reactives: Set[ReSource[S]]): Unit
  def outgoing(): Iterable[Reactive[S]]
  def discover(reactive: Reactive[S]): Unit
  def drop(reactive: Reactive[S]): Unit
}

case class Token(payload: AnyRef = null)


/**
  * Spore that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update
  *
  * @tparam P Pulse stored value type
  */
trait ReadWriteValue[P, S <: Struct, N] extends Committable[S] {
  def base(token: Token): P
  def get(token: Token): P
  def write(value: P, token: Token): Boolean
  def notification(token: Token): N
  def setNotificaiton(notification: N, token: Token): Boolean
}



