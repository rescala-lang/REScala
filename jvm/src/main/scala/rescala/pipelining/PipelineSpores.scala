package rescala.pipelining

import rescala.graph.Spores.TraitStructP
import rescala.graph.{Spores, Pulse, Reactive, Buffer}
import rescala.synchronization.TurnLock
import rescala.turns.Turn

object PipelineSpores extends Spores {
  override type Struct = PipelineStructP[_]

  override def bud[P](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true): StructP[P] = new PipelineStructP[P](initialValue, transient)

  class PipelineStructP[P](initialValue: Pulse[P], transient: Boolean) extends TraitStructP[P] {

    val pipeline: Pipeline = new Pipeline(???)

    private val _incoming: BlockingPipelineBuffer[Set[Reactive[_]]] = pipeline.createBlockingBuffer(Set.empty, Buffer.commitAsIs)
    override def incoming[S <: Spores](implicit turn: Turn[S]): Set[Reactive[S]] = _incoming.get.asInstanceOf[Set[Reactive[S]]]
    def incomingForceGet[S <: Spores](implicit turn: Turn[S]): Set[Reactive[S]] = _incoming.forceGet.asInstanceOf[Set[Reactive[S]]]
    override def updateIncoming[S <: Spores](reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit = _incoming.set(reactives.toSet)


    private val lvl: NonblockingPipelineBuffer[Int] = pipeline.createNonblockingBuffer(0, math.max)
    override def level(implicit turn: Turn[_]): Int = lvl.get
    override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = lvl.transform(math.max(i, _))


    private val _outgoing: NonblockingPipelineBuffer[Set[Reactive[_]]] = pipeline.createNonblockingBuffer(Set.empty, Buffer.commitAsIs)
    override def outgoing[S <: Spores](implicit turn: Turn[S]): Set[Reactive[S]] = _outgoing.get.asInstanceOf[Set[Reactive[S]]]
    override def discover[S <: Spores](reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing.transform(_ + reactive)
    override def drop[S <: Spores](reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing.transform(_ - reactive)


    override val pulses: Buffer[Pulse[P]] = pipeline.createBlockingBuffer(initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse)
  }
}