package rescala.twoversion

import rescala.core.{Derived, ReSource, Struct}

import scala.collection.mutable

trait TwoVersionStruct extends Struct {
  override type State[V, S <: Struct] <: TwoVersionState[V, S]
}

case class Token(payload: AnyRef = null)

/** State that implements both the buffered pulse and the buffering capabilities itself. */
abstract class TwoVersionState[V, S <: Struct](protected[rescala] var current: V) extends Committable[V] {

  private var owner: Token = null
  private var update: V    = _

  def write(value: V, token: Token): Boolean = {
    assert(owner == null || owner == token, s"buffer owned by $owner written by $token")
    update = value
    val res = owner == null
    owner = token
    res
  }
  def base(token: Token): V = current
  def get(token: Token): V = { if (token eq owner) update else current }

  override def commit(r: V => V): Unit = {
    current = r(update)
    release()
  }
  override def release(): Unit = {
    update = null.asInstanceOf[V]
    owner = null
  }

  /* incoming and outgoing changes */

  var incoming: Set[ReSource[S]]                              = Set.empty
  protected var _outgoing: mutable.Map[Derived[S], None.type] = mutable.HashMap.empty

  def updateIncoming(reactives: Set[ReSource[S]]): Unit = incoming = reactives
  def outgoing(): Iterable[Derived[S]]                  = _outgoing.keys
  def discoveredBy(reactive: Derived[S]): Unit          = _outgoing.put(reactive, None)
  def droppedBy(reactive: Derived[S]): Unit             = _outgoing -= reactive
}
