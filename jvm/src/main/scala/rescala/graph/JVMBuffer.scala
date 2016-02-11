package rescala.graph

import rescala.synchronization.{TurnLock, Key, ParRP, STMSync}
import rescala.turns.Turn

import scala.concurrent.stm.{InTxn, Ref}
import scala.language.implicitConversions


object ParRPSpores extends Spores {
  override type Bud[P] = ParRPBud[P]

  override def bud[P](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true): Bud[P] = {
    val lock = new TurnLock
    new ParRPBud[P](initialValue, transient, lock)
  }

  class ParRPBud[P](var current: Pulse[P], transient: Boolean, val lock: TurnLock) extends TraitBud[P] with Buffer[Pulse[P]] with Committable {

    private var _incoming: Set[Reactive[_]] = Set.empty
    override def incoming[S <: Spores](implicit turn: Turn[S]): Set[Reactive[S]] = _incoming.asInstanceOf[Set[Reactive[S]]]
    override def updateIncoming[S <: Spores](reactives: Set[Reactive[S]])(implicit turn: Turn[S]): Unit = _incoming = reactives.toSet


    private var lvl: Int = 0

    override def level(implicit turn: Turn[_]): Int = lvl
    override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
      lvl = math.max(level, i)
      lvl
    }

    private var _outgoing: Set[Reactive[_]] = Set[Reactive[_]]()
    override def outgoing[S <: Spores](implicit turn: Turn[S]): Set[Reactive[S]] = _outgoing.asInstanceOf[Set[Reactive[S]]]
    override def discover[S <: Spores](reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing += reactive
    override def drop[S <: Spores](reactive: Reactive[S])(implicit turn: Turn[S]): Unit = _outgoing -= reactive



    override val pulses: Buffer[Pulse[P]] = this

    private var update: Pulse[P] = Pulse.none
    private var owner: Turn[_] = null

    override def transform(f: (Pulse[P]) => Pulse[P])(implicit turn: Turn[_]): Pulse[P] =  {
      val value = f(get)
      set(value)
      value
    }

    override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit =  {
      assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
      turn match {
        case pessimistic: ParRP =>
          val wlo: Option[Key] = Option(lock).map(_.getOwner)
          assert(wlo.fold(true)(_ eq pessimistic.key),
            s"buffer owned by $owner, controlled by $lock with owner ${wlo.get}" +
              s" was written by $turn who locks with ${pessimistic.key}, by now the owner is ${lock.getOwner}")
        case _ =>
          throw new IllegalStateException(s"parrp buffer used with wrong turn")
      }
      update = value
      if (owner == null) turn.schedule(this)
      owner = turn
    }

    override def base(implicit turn: Turn[_]): Pulse[P] = current

    override def get(implicit turn: Turn[_]): Pulse[P] =  {if (turn eq owner) update else current}

    override def release(implicit turn: Turn[_]): Unit =  {
      update = Pulse.none
      owner = null
    }

    override def commit(implicit turn: Turn[_]): Unit =  {
      if (!transient) current = update.keep
      release(turn)
    }


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
