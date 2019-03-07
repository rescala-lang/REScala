package rescala.twoversion

import rescala.core.Initializer.InitValues
import rescala.core.{ReSource, Derived, Struct}

import scala.language.higherKinds


trait TwoVersionStruct extends Struct {
  override type State[P, S <: Struct] <: GraphState[S] with ReadWriteValue[P, S]
}

case class Token(payload: AnyRef = null)


/** State that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update */
trait ReadWriteValue[P, S <: Struct] extends Committable[S] {
  def base(token: Token): P
  def get(token: Token): P
  def write(value: P, token: Token): Boolean
}

/** State representing a node in a graph by providing information about incoming and outgoing edges. */
trait GraphState[S <: Struct] {
  def incoming(): Set[ReSource[S]]
  def updateIncoming(reactives: Set[ReSource[S]]): Unit
  def outgoing(): Iterable[Derived[S]]
  def discover(reactive: Derived[S]): Unit
  def drop(reactive: Derived[S]): Unit
}

/** State that implements both the buffered pulse and the buffering capabilities itself. */
class BufferedValueState[V, S <: Struct](ip: InitValues[V]) extends ReadWriteValue[V, S] with Committable[S] {
  var current: V = ip.initialValue
  protected var owner: Token = null
  private var update: V = _

  override def write(value: V, token: Token): Boolean = {
    assert(owner == null || owner == token, s"buffer owned by $owner written by $token")
    update = value
    val res = owner == null
    owner = token
    res
  }
  override def base(token: Token): V = current
  override def get(token: Token): V = {if (token eq owner) update else current}

  override def commit(): Unit = {
    current = ip.unchange.unchange(update)
    release()
  }
  override def release(): Unit = {
    update = null.asInstanceOf[V]
    owner = null
  }
}

/**  Implementation of a struct with graph functionality and a buffered pulse storage.  */
abstract class GraphStateImpl[P, S <: Struct](ip: InitValues[P]) extends BufferedValueState[P, S](ip) with GraphState[S] {
  protected var _incoming: Set[ReSource[S]]                                    = Set.empty
  protected var _outgoing: scala.collection.mutable.Map[Derived[S], None.type] = rescala.util.WeakHashMap.empty


  override def incoming(): Set[ReSource[S]] = _incoming
  override def updateIncoming(reactives: Set[ReSource[S]]): Unit = _incoming = reactives
  override def outgoing(): Iterable[Derived[S]] = _outgoing.keys
  override def discover(reactive: Derived[S]): Unit = _outgoing.put(reactive, None)
  override def drop(reactive: Derived[S]): Unit = _outgoing -= reactive
}




