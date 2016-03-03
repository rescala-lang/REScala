package rescala.graph

import rescala.propagation.{Committable, Turn}

import scala.language.{existentials, higherKinds, implicitConversions}

object Buffer {
  type CommitStrategy[A] = (A, A) => A
  def commitAsIs[A](base: A, cur: A): A = cur
  def transactionLocal[A](base: A, cur: A) = base
  def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = cur.keep
}

trait Buffer[A] {
  def transform(f: (A) => A)(implicit turn: Turn[_]): A
  def set(value: A)(implicit turn: Turn[_]): Unit
  def base(implicit turn: Turn[_]): A
  def get(implicit turn: Turn[_]): A
}


trait Struct {
  final type Spore[R] = SporeP[_, R]
  type SporeP[P, R]

}

trait PulsingGraphStruct extends Struct {
  override type SporeP[P, R] <: GraphSpore[R] with PulsingSpore[P]
}

trait LevelStruct extends PulsingGraphStruct {
  override type SporeP[P, R] <: LevelSpore[R] with GraphSpore[R] with PulsingSpore[P]
}

trait SimpleStruct[S] extends LevelStruct { override type SporeP[P, R] = LevelSporeImpl[P, R] }


trait PulsingSpore[P] {
  def pulses: Buffer[Pulse[P]]
}

trait GraphSpore[R] {
  def incoming(implicit turn: Turn[_]): Set[R]
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit
  def outgoing(implicit turn: Turn[_]): Set[R]
  def discover(reactive: R)(implicit turn: Turn[_]): Unit
  def drop(reactive: R)(implicit turn: Turn[_]): Unit
}

trait LevelSpore[R] extends GraphSpore[R] {

  def level(implicit turn: Turn[_]): Int
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int

}


abstract class PropagationSporeImpl[P, R](var current: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends GraphSpore[R] with PulsingSpore[P] with Buffer[Pulse[P]] with Committable {

  val pulses: Buffer[Pulse[P]] = this
  var _incoming: Set[R] = initialIncoming
  var _outgoing: Set[R] = Set.empty
  protected var owner: Turn[_] = null
  private var update: Pulse[P] = Pulse.none

  def incoming(implicit turn: Turn[_]): Set[R] = _incoming
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives
  override def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing += reactive
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing -= reactive


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
  override def get(implicit turn: Turn[_]): Pulse[P] = { if (turn eq owner) update else current }
  override def base(implicit turn: Turn[_]): Pulse[P] = current

  override def commit(implicit turn: Turn[_]): Unit = {
    if (!transient) current = update.keep
    release(turn)
  }
  override def release(implicit turn: Turn[_]): Unit = {
    update = Pulse.none
    owner = null
  }
}

class LevelSporeImpl[P, R](current: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends PropagationSporeImpl[P, R](current, transient, initialIncoming) with LevelSpore[R]  {
  var _level: Int = 0

  override def level(implicit turn: Turn[_]): Int = _level

  override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }


}
