package rescala.graph

import rescala.graph.Buffer.CommitStrategy
import rescala.graph.Spores.{TraitStruct, TraitStructP}
import rescala.turns.Turn

import scala.language.{existentials, higherKinds, implicitConversions}

trait Committable {
  def commit(implicit turn: Turn[_]): Unit
  def release(implicit turn: Turn[_]): Unit
}

object Buffer {
  type CommitStrategy[A] = (A, A) => A
  def commitAsIs[A](base: A, cur: A): A = cur
  def transactionLocal[A](base: A, cur: A) = base
  def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = cur.keep
}

trait Spores {
  type Struct[R] <: TraitStruct[R]
  type StructP[P, R] = Struct[R] with TraitStructP[P, R]

  def bud[P, R](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): StructP[P, R]

}

object Spores {

  trait TraitStruct[R] {

    def level(implicit turn: Turn[_]): Int
    def updateLevel(i: Int)(implicit turn: Turn[_]): Int

    def incoming(implicit turn: Turn[_]): Set[R]
    def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit


    def outgoing(implicit turn: Turn[_]): Set[R]
    def discover(reactive: R)(implicit turn: Turn[_]): Unit
    def drop(reactive: R)(implicit turn: Turn[_]): Unit

  }
  trait TraitStructP[P, R] extends TraitStruct[R] {

    val pulses: Buffer[Pulse[P]]

  }


}

trait BufferedSpores extends Spores {
  abstract class BufferedStructP[P, R](initialIncoming: Set[R]) extends TraitStructP[P, R] {
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
}

object SimpleSpores extends BufferedSpores {
  override type Struct[R] = SimpleStructP[_, R]

  def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): StructP[P, R] =
    new SimpleStructP[P, R](new SimpleBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse), initialIncoming)

  class SimpleStructP[P, R](override val pulses: SimpleBuffer[Pulse[P]], initialIncoming: Set[R]) extends BufferedStructP[P, R](initialIncoming) {
    override def buffer[A](default: A, commitStrategy: CommitStrategy[A]): SimpleBuffer[A] = new SimpleBuffer[A](default, commitStrategy)
  }
}

trait Buffer[A] {
  def transform(f: (A) => A)(implicit turn: Turn[_]): A
  def set(value: A)(implicit turn: Turn[_]): Unit
  def base(implicit turn: Turn[_]): A
  def get(implicit turn: Turn[_]): A
}

final class SimpleBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] with Committable {

  var current: A = initialValue
  private var update: Option[A] = None
  private var owner: Turn[_] = null
  private val commitStrategy: (A, A) => A = initialStrategy

  override def transform(f: (A) => A)(implicit turn: Turn[_]): A = synchronized {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn[_]): Unit = synchronized {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    update = Some(value)
    owner = turn
    turn.schedule(this)
  }

  override def base(implicit turn: Turn[_]): A = synchronized(current)

  override def get(implicit turn: Turn[_]): A = synchronized {if (turn eq owner) update.getOrElse(current) else current}

  override def release(implicit turn: Turn[_]): Unit = synchronized {
    update = None
    owner = null
  }

  override def commit(implicit turn: Turn[_]): Unit = synchronized {
    current = commitStrategy(current, get)
    release(turn)
  }
}
