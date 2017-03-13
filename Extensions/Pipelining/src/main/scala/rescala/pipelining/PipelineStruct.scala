package rescala.pipelining

import rescala.graph._
import rescala.propagation.Turn
import rescala.twoversion.Committable

object PipelineStruct extends Struct {
  override type Type[P, R] = PipelineSporeP[P, R]

}

/**
 * Buffer companion object. Only used for pipelining.
 */
object Buffer {
  type CommitStrategy[A] = (A, A) => A
  def commitAsIs[A](base: A, cur: A): A = cur
  def transactionLocal[A](base: A, cur: A) = base
  def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = if (cur.isChange) cur else base
}

/**
 * Buffer that stores a temporary new value for its content until it is committed.
 *
 * @tparam A Buffer stored content type
 */
trait Buffer[A] extends Committable {
  def transform(f: (A) => A)(implicit turn: Turn[_]): A
  def set(value: A)(implicit turn: Turn[_]): Unit
  def base(implicit turn: Turn[_]): A
  def get(implicit turn: Turn[_]): A
}

class PipelineSporeP[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends ReadWriteValue[P] with GraphStructType[R] {

  val pipeline: Pipeline = new Pipeline()

  private val _incoming: BlockingPipelineBuffer[Set[R]] = pipeline.createBlockingBuffer(initialIncoming, Buffer.commitAsIs)
  def incoming(implicit turn: Turn[_]): Set[R] = _incoming.get
  def incomingForceGet(implicit turn: Turn[_]): Set[R] = _incoming.forceGet
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming.set(reactives.toSet)

  private val lvl: NonblockingPipelineBuffer[Int] = pipeline.createNonblockingBuffer(0, math.max)
  def level(implicit turn: Turn[_]): Int = lvl.get
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int = lvl.transform(math.max(i, _))

  private val _outgoing: NonblockingPipelineBuffer[Set[R]] = pipeline.createNonblockingBuffer(Set.empty, Buffer.commitAsIs)
  def outgoing(implicit turn: Turn[_]): Iterator[R] = _outgoing.get.iterator
  def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transform(_ + reactive)
  def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transform(_ - reactive)

  val delegate = pipeline.createBlockingBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse)
  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = delegate.set(value)
  override def base(implicit turn: Turn[_]): Pulse[P] = delegate.base
  override def get(implicit turn: Turn[_]): Pulse[P] = delegate.get
}
