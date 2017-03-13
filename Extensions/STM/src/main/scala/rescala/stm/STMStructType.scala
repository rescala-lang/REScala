package rescala.stm

import rescala.graph._
import rescala.levelbased.LevelStructType
import rescala.propagation.Turn

import scala.concurrent.stm.{InTxn, Ref, TxnLocal}

class STMStructType[P, S <: Struct](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[Reactive[S]]) extends LevelStructType[S] with ReadWriteValue[P, S] {

  implicit def inTxn(implicit turn: Turn[S]): InTxn = turn match {
    case stmTurn: STMTurn => stmTurn.inTxn
    case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
  }

  val _level: Ref[Int] = Ref(0)
  val _outgoing: Ref[Set[Reactive[S]]] = Ref(Set.empty)
  val _incoming: Ref[Set[Reactive[S]]] = Ref(initialIncoming)

  val pulses: ReadWriteValue[P, S] = this
  def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = _incoming.get
  override def level(implicit turn: Turn[S]): Int = _level.get
  override def drop(reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing.transformAndGet(_ - reactive)
  override def updateLevel(i: Int)(implicit turn: Turn[S]): Int = _level.transformAndGet(math.max(_, i))
  override def outgoing(implicit turn: Turn[S]): Iterator[Reactive[S]] = _outgoing.get.iterator
  def updateIncoming(reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit = _incoming.set(reactives)
  override def discover(reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing.transformAndGet(_ + reactive)


  private val current: Ref[Pulse[P]] = Ref(initialValue)
  private val update: TxnLocal[Option[Pulse[P]]] = TxnLocal(None,beforeCommit = { implicit inTxn =>
    val updateValue: Option[Pulse[P]] = update.get
    if (!transient && updateValue.isDefined && updateValue.get.isChange) current.set(updateValue.get)
  })

  override def set(value: Pulse[P])(implicit turn: S#Ticket[S]): Unit = {
    update.set(Some(value))(inTxn(turn.turn()))
  }
  override def base(implicit turn: S#Ticket[S]) = current.get(inTxn(turn.turn()))
  override def get(implicit turn: S#Ticket[S]): Pulse[P] = update.get(inTxn(turn.turn())).getOrElse(current.get(inTxn(turn.turn())))

}
