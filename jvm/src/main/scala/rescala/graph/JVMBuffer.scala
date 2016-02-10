package rescala.graph

import rescala.synchronization.{TurnLock, Key, ParRP, STMSync}
import rescala.turns.Turn

import scala.concurrent.stm.{InTxn, Ref}
import scala.language.implicitConversions


object ParRPSpores extends Spores {
  override type Bud[P] = ParRPBud[P]

  override def bud[P](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true): Bud[P] = {
    val lock = new TurnLock
    new ParRPBud[P](lock, new ParRPBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse, lock))
  }

  class ParRPBud[P](val lock: TurnLock, override val pulses: ParRPBuffer[Pulse[P]]) extends TraitBud[P] {

    private val _incoming: Buffer[Set[Reactive[_]]] = new ParRPBuffer[Set[Reactive[_]]](Set.empty, Buffer.commitAsIs, lock)
    override def incoming(implicit turn: Turn[_]): Set[Reactive[_]] = _incoming.get
    override def updateIncoming[S <: Spores](reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit = _incoming.set(reactives.toSet)


    private var lvl: Int = 0

    override def level(implicit turn: Turn[_]): Int = lvl
    override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
      lvl = math.max(level, i)
      lvl
    }

    private var _outgoing: Set[Reactive[_]] = Set[Reactive[_]]()
    override def outgoing(implicit turn: Turn[_]): Set[Reactive[_]] = _outgoing
    override def discover[S <: Spores](reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing += reactive
    override def drop[S <: Spores](reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing -= reactive
  }
}

object STMSpores extends BufferedSpores {
  override type Bud[P] = STMBud[P]

  override def bud[P](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true): Bud[P] = {
    new STMBud[P](new STMBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse))
  }

  class STMBud[P](override val pulses: STMBuffer[Pulse[P]]) extends BufferedBud[P] {
    override def buffer[A](default: A, commitStrategy: (A, A) => A): STMBuffer[A] = new STMBuffer[A](default, commitStrategy)
  }

}

final class ParRPBuffer[A](initialValue: A, initialStrategy: (A, A) => A, writeLock: TurnLock) extends Buffer[A] with Committable {

  var current: A = initialValue
  private var update: Option[A] = None
  private var owner: Turn[_] = null
  private val commitStrategy: (A, A) => A = initialStrategy

  override def transform(f: (A) => A)(implicit turn: Turn[_]): A =  {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn[_]): Unit =  {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    turn match {
      case pessimistic: ParRP =>
        val wlo: Option[Key] = Option(writeLock).map(_.getOwner)
        assert(wlo.fold(true)(_ eq pessimistic.key),
          s"buffer owned by $owner, controlled by $writeLock with owner ${wlo.get}" +
            s" was written by $turn who locks with ${pessimistic.key}, by now the owner is ${writeLock.getOwner}")
      case _ =>
        throw new IllegalStateException(s"parrp buffer used with wrong turn")
    }
    update = Some(value)
    owner = turn
    turn.schedule(this)
  }

  override def base(implicit turn: Turn[_]): A = current

  override def get(implicit turn: Turn[_]): A =  {if (turn eq owner) update.getOrElse(current) else current}

  override def release(implicit turn: Turn[_]): Unit =  {
    update = None
    owner = null
  }

  override def commit(implicit turn: Turn[_]): Unit =  {
    current = commitStrategy(current, get)
    release(turn)
  }
}

final class STMBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] with Committable {

  private val current: Ref[A] = Ref(initialValue)
  private val update: Ref[Option[A]] = Ref(None)
  private val commitStrategy: (A, A) => A = initialStrategy

  implicit def inTxn(implicit turn: Turn[_]): InTxn = turn match {
    case stmTurn: STMSync => stmTurn.inTxn
    case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
  }

  override def transform(f: (A) => A)(implicit turn: Turn[_]): A = {
    val value = f(get)
    set(value)
    value
  }
  override def set(value: A)(implicit turn: Turn[_]): Unit = {
    update.set(Some(value))
    turn.schedule(this)
  }
  override def base(implicit turn: Turn[_]) = current.get
  override def get(implicit turn: Turn[_]): A = update.get.getOrElse(current.get)
  override def release(implicit turn: Turn[_]): Unit = {
    update.set(None)
  }
  override def commit(implicit turn: Turn[_]): Unit = {
    current.set(commitStrategy(current.get, get))
    release
  }
}
