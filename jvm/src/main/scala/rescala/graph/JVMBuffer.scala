package rescala.graph

import rescala.graph.Spores.TraitStructP
import rescala.synchronization.{TurnLock, Key, ParRP, STMSync}
import rescala.turns.Turn

import scala.concurrent.stm.{InTxn, Ref}
import scala.language.implicitConversions


object ParRPSpores extends Spores {
  override type Struct[R] = ParRPStructP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): StructP[P, R] = {
    val lock = new TurnLock
    new ParRPStructP[P, R](initialValue, transient, lock, initialIncoming)
  }

  class ParRPStructP[P, R](var current: Pulse[P], transient: Boolean, val lock: TurnLock, initialIncoming: Set[R]) extends TraitStructP[P, R] with Buffer[Pulse[P]] with Committable {

    private var _incoming: Set[R] = initialIncoming
    override def incoming(implicit turn: Turn[_]): Set[R] = _incoming
    override def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives.toSet


    private var lvl: Int = 0

    override def level(implicit turn: Turn[_]): Int = lvl
    override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
      lvl = math.max(level, i)
      lvl
    }

    private var _outgoing: Set[R] = Set[R]()
    override def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing
    override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing += reactive
    override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing -= reactive



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
  override type Struct[R] = STMStructP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): StructP[P, R] = {
    new STMStructP[P, R](new STMBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse), initialIncoming)
  }

  class STMStructP[P, R](override val pulses: STMBuffer[Pulse[P]], initialIncoming: Set[R]) extends BufferedStructP[P, R](initialIncoming) {
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
