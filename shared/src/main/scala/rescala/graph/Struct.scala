package rescala.graph

import rescala.graph.Buffer.CommitStrategy
import rescala.graph.Struct.{TraitSpore, TraitSporeP}
import rescala.propagation.Turn

import scala.language.{existentials, higherKinds, implicitConversions}


trait Struct {
  type Spore[R] <: TraitSpore[R]
  type SporeP[P, R] = Spore[R] with TraitSporeP[P, R]

  def bud[P, R](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): SporeP[P, R]

}

object Struct {

  trait TraitSpore[R] {

    def level(implicit turn: Turn[_]): Int
    def updateLevel(i: Int)(implicit turn: Turn[_]): Int

    def incoming(implicit turn: Turn[_]): Set[R]
    def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit


    def outgoing(implicit turn: Turn[_]): Set[R]
    def discover(reactive: R)(implicit turn: Turn[_]): Unit
    def drop(reactive: R)(implicit turn: Turn[_]): Unit

  }
  trait TraitSporeP[P, R] extends TraitSpore[R] {

    val pulses: Buffer[Pulse[P]]

  }


}

trait BufferedStruct extends Struct {
  abstract class BufferedSporeP[P, R](initialIncoming: Set[R]) extends TraitSporeP[P, R] {
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

object SimpleStruct extends BufferedStruct {
  override type Spore[R] = SimpleSporeP[_, R]

  def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] =
    new SimpleSporeP[P, R](new SimpleBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse), initialIncoming)

  class SimpleSporeP[P, R](override val pulses: SimpleBuffer[Pulse[P]], initialIncoming: Set[R]) extends BufferedSporeP[P, R](initialIncoming) {
    override def buffer[A](default: A, commitStrategy: CommitStrategy[A]): SimpleBuffer[A] = new SimpleBuffer[A](default, commitStrategy)
  }
}
