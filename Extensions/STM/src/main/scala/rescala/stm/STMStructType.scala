package rescala.stm

import rescala.engine.Turn
import rescala.graph.{Reactive, Struct}
import rescala.levelbased.LevelStructType
import rescala.twoversion.{ReadWriteValue, Token, TwoVersionPropagation}

import scala.concurrent.stm.{InTxn, Ref, TxnLocal, atomic}

class STMStructType[P, S <: Struct](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[S]]) extends LevelStructType[S] with ReadWriteValue[P, S] {

  implicit def inTxn(implicit turn: Turn[S]): InTxn = turn match {
    case stmTurn: STMTurn => stmTurn.inTxn
    case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
  }

  def inTxn(token: Token): InTxn = token.payload match {
    case stmTurn: InTxn => stmTurn
    case _ => throw new IllegalStateException(s"$token has invalid type for $this")
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


  private val current: Ref[P] = Ref(initialValue)
  private val update: TxnLocal[Option[P]] = TxnLocal(None,beforeCommit = { implicit inTxn =>
    val updateValue: Option[P] = update.get
    if (!transient && updateValue.isDefined) current.set(updateValue.get)
  })

  override def write(value: P, token: Token): Boolean = {
    update.set(Some(value))(inTxn(token))
    false
  }
  override def base(token: Token): P = current.get(inTxn(token))
  override def get(token: Token): P = update.get(inTxn(token)).getOrElse(current.get(inTxn(token)))


  override def commit(implicit turn: TwoVersionPropagation[S]): Unit = {}
  override def release(implicit turn: TwoVersionPropagation[S]): Unit = {}
}
