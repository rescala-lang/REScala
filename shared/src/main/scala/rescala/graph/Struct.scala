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
  type Spore[R] <: ReactiveSpore[R]
  type SporeP[P, R] = Spore[R] with SporePulse[P]

  def bud[P, R](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): SporeP[P, R]

}

trait ReactiveSpore[R] {

  def incoming(implicit turn: Turn[_]): Set[R]
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit

}
trait SporePulse[P] {

  val pulses: Buffer[Pulse[P]]

}

trait PropagationSpore[R] extends ReactiveSpore[R] {

  def outgoing(implicit turn: Turn[_]): Set[R]
  def discover(reactive: R)(implicit turn: Turn[_]): Unit
  def drop(reactive: R)(implicit turn: Turn[_]): Unit
}

trait PropagationStruct extends Struct {
  override type Spore[R] <: PropagationSpore[R]
}

trait LevelSpore[R] extends PropagationSpore[R] {

  def level(implicit turn: Turn[_]): Int
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int

}

trait LevelStruct extends Struct {
  override type Spore[R] <: LevelSpore[R]
}


object SimpleStruct extends LevelStruct {
  override type Spore[R] = LevelSporeImpl[_, R]

  def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] =
    new LevelSporeImpl[P, R](initialValue, transient, initialIncoming)
}

abstract class PropagationSporeImpl[P, R](var current: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends PropagationSpore[R] with SporePulse[P] with Buffer[Pulse[P]] with Committable {

  override val pulses: Buffer[Pulse[P]] = this
  var _incoming: Set[R] = initialIncoming
  var _outgoing: Set[R] = Set.empty
  protected var owner: Turn[_] = null
  private var update: Pulse[P] = Pulse.none

  override def incoming(implicit turn: Turn[_]): Set[R] = _incoming
  override def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives
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
