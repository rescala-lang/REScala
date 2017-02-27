package rescala.stm

import rescala.graph._
import rescala.levelbased.LevelStructType
import rescala.propagation.Turn

import scala.concurrent.stm.{InTxn, Ref, TxnLocal}

class STMStructType[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends LevelStructType[R] with PulseStruct[P] {

  implicit def inTxn(implicit turn: Turn[_]): InTxn = turn match {
    case stmTurn: STMTurn => stmTurn.inTxn
    case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
  }

  val _level: Ref[Int] = Ref(0)
  val _outgoing: Ref[Set[R]] = Ref(Set.empty)
  val _incoming: Ref[Set[R]] = Ref(initialIncoming)

  val pulses: PulseStruct[P] = this
  def incoming(implicit turn: Turn[_]): Set[R] = _incoming.get
  override def level(implicit turn: Turn[_]): Int = _level.get
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transformAndGet(_ - reactive)
  override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = _level.transformAndGet(math.max(_, i))
  override def outgoing(implicit turn: Turn[_]): Iterator[R] = _outgoing.get.iterator
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming.set(reactives)
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transformAndGet(_ + reactive)


  private val current: Ref[Pulse[P]] = Ref(initialValue)
  private val update: TxnLocal[Option[Pulse[P]]] = TxnLocal(None,beforeCommit = { implicit inTxn =>
    val updateValue: Option[Pulse[P]] = update.get
    if (!transient && updateValue.isDefined && updateValue.get.isChange) current.set(updateValue.get)
  })

  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    update.set(Some(value))
  }
  override def base(implicit turn: Turn[_]) = current.get
  override def get(implicit turn: Turn[_]): Pulse[P] = update.get.getOrElse(current.get)

}
