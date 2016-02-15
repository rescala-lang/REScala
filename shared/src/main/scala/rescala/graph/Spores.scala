package rescala.graph

import Spores.{TraitStructP, TraitStruct}
import rescala.graph.Buffer.CommitStrategy
import rescala.propagation.Turn

import scala.language.{existentials, higherKinds, implicitConversions}


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
