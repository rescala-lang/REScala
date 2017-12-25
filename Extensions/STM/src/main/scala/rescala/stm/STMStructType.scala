package rescala.stm

import rescala.core.{ReSource, Reactive, Struct, Turn}
import rescala.levelbased.LevelStructType
import rescala.twoversion.{ReadWriteValue, Token, TwoVersionPropagation}

import scala.concurrent.stm.{InTxn, Ref, TxnLocal}

class STMStructType[P, S <: Struct](initialValue: P, transient: Boolean) extends LevelStructType[S] with ReadWriteValue[P, S] {

  def inTxn(turn: Turn[S]): InTxn = turn match {
    case stmTurn: STMTurn => stmTurn.inTxn
    case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
  }

  def inTxn(token: Token): InTxn = token.payload match {
    case stmTurn: InTxn => stmTurn
    case _ => throw new IllegalStateException(s"$token has invalid type for $this")
  }


  val _level: Ref[Int] = Ref(0)
  val _outgoing: Ref[Set[Reactive[S]]] = Ref(Set.empty)
  val _incoming: Ref[Set[ReSource[S]]] = Ref(Set.empty)

  val pulses: ReadWriteValue[P, S] = this
  def incoming(turn: Turn[S]): Set[ReSource[S]] = _incoming.get(inTxn(turn))
  override def level(turn: Turn[S]): Int = _level.get(inTxn(turn))
  override def drop(reactive: Reactive[S])(turn: Turn[S]): Unit = _outgoing.transformAndGet(_ - reactive)(inTxn(turn))
  override def updateLevel(i: Int)(turn: Turn[S]): Int = _level.transformAndGet(math.max(_, i))(inTxn(turn))
  override def outgoing(turn: Turn[S]): Iterator[Reactive[S]] = _outgoing.get(inTxn(turn)).iterator
  def updateIncoming(reactives: Set[ReSource[S]])(turn: Turn[S]): Unit = _incoming.set(reactives)(inTxn(turn))
  override def discover(reactive: Reactive[S])(turn: Turn[S]): Unit = _outgoing.transformAndGet(_ + reactive)(inTxn(turn))


  private val current: Ref[P] = Ref(initialValue)
  private val update: TxnLocal[Option[P]] = TxnLocal(None,beforeCommit = { implicit inTxn =>
    val updateValue: Option[P] = update.get
    if (!transient && updateValue.isDefined) current.set(updateValue.get)
  })

  override def write(value: P, token: Token): Boolean = {
    update.set(Some(value))(inTxn(token))
    false
  }
  override def base(token: Token): P = if(token == null) {
    current.single.get
  } else {
    current.get(inTxn(token))
  }
  override def get(token: Token): P = update.get(inTxn(token)).getOrElse(current.get(inTxn(token)))


  override def commit(turn: TwoVersionPropagation[S]): Unit = {}
  override def release(turn: TwoVersionPropagation[S]): Unit = {}
}
