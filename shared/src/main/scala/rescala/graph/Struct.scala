package rescala.graph

import rescala.graph.Buffer.CommitStrategy
import rescala.propagation.Turn

import scala.language.{existentials, higherKinds, implicitConversions}


trait Struct {
  type Spore[R] <: ReactiveSpore[R]
  type SporeP[P, R] = Spore[R] with PulseSpore[P]

  def bud[P, R](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): SporeP[P, R]

}

trait ReactiveSpore[R] {

  def incoming(implicit turn: Turn[_]): Set[R]
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit

}
trait PulseSpore[P] {

  val pulses: Buffer[Pulse[P]]

}

trait LevelSpore[R] extends ReactiveSpore[R] {

  def level(implicit turn: Turn[_]): Int
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int

  def outgoing(implicit turn: Turn[_]): Set[R]
  def discover(reactive: R)(implicit turn: Turn[_]): Unit
  def drop(reactive: R)(implicit turn: Turn[_]): Unit
}


trait LevelStruct extends Struct {
  type Spore[R] <: LevelSpore[R]
}

abstract class BufferedSporeP[P, R](initialIncoming: Set[R]) extends LevelSpore[R] with PulseSpore[P] {
  def buffer[A](default: A, commitStrategy: CommitStrategy[A]): Buffer[A]
  private val _level: Buffer[Int] = buffer(0, math.max)
  private val _incoming: Buffer[Set[R]] = buffer(initialIncoming, Buffer.commitAsIs)
  private val _outgoing: Buffer[Set[R]] = buffer(Set.empty, Buffer.commitAsIs)

  override def level(implicit turn: Turn[_]): Int = _level.get(turn)
  override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = _level.transform(math.max(i, _))

  override def incoming(implicit turn: Turn[_]): Set[R] = _incoming.get
  override def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming.set(reactives.toSet)


  override def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing.get
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transform(_ + reactive)
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transform(_ - reactive)

}

object SimpleStruct extends LevelStruct {
  override type Spore[R] = SimpleSporeP[_, R]

  def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] =
    new SimpleSporeP[P, R](initialValue, transient, initialIncoming)
}

class SimpleSporeP[P, R](var current: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends LevelSpore[R] with PulseSpore[P] with Buffer[Pulse[P]] with Committable {
  var _level: Int = 0
  var _incoming: Set[R] = initialIncoming
  var _outgoing: Set[R] = Set.empty
  override def level(implicit turn: Turn[_]): Int = _level
  override def incoming(implicit turn: Turn[_]): Set[R] = _incoming
  override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing -= reactive
  override def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing
  override def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing += reactive

  override val pulses: Buffer[Pulse[P]] = this

  private var update: Pulse[P] = Pulse.none
  protected var owner: Turn[_] = null

  override def transform(f: (Pulse[P]) => Pulse[P])(implicit turn: Turn[_]): Pulse[P] = {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    update = value
    if (owner == null) turn.schedule(this)
    owner = turn
  }

  override def base(implicit turn: Turn[_]): Pulse[P] = current

  override def get(implicit turn: Turn[_]): Pulse[P] = { if (turn eq owner) update else current }

  override def release(implicit turn: Turn[_]): Unit = {
    update = Pulse.none
    owner = null
  }

  override def commit(implicit turn: Turn[_]): Unit = {
    if (!transient) current = update.keep
    release(turn)
  }

}
