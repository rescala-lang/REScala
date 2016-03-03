package rescala.pipelining

import rescala.graph._
import rescala.propagation.Turn

object PipelineStruct extends Struct {
  override type SporeP[P, R] = PipelineSporeP[P, R]

}

class PipelineSporeP[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]) {

  val pipeline: Pipeline = new Pipeline()

  private val _incoming: BlockingPipelineBuffer[Set[R]] = pipeline.createBlockingBuffer(initialIncoming, Buffer.commitAsIs)
  def incoming(implicit turn: Turn[_]): Set[R] = _incoming.get
  def incomingForceGet(implicit turn: Turn[_]): Set[R] = _incoming.forceGet
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming.set(reactives.toSet)


  private val lvl: NonblockingPipelineBuffer[Int] = pipeline.createNonblockingBuffer(0, math.max)
  def level(implicit turn: Turn[_]): Int = lvl.get
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int = lvl.transform(math.max(i, _))


  private val _outgoing: NonblockingPipelineBuffer[Set[R]] = pipeline.createNonblockingBuffer(Set.empty, Buffer.commitAsIs)
  def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing.get
  def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transform(_ + reactive)
  def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transform(_ - reactive)


  val pulses: Buffer[Pulse[P]] = pipeline.createBlockingBuffer(initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse)
}