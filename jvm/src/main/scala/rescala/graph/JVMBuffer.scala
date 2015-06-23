package rescala.graph

import rescala.synchronization.{TurnLock, Key, ParRP, STMSync}
import rescala.turns.Turn

import scala.concurrent.stm.{InTxn, Ref}
import scala.language.implicitConversions

object JVMFactories {

  class ParRPState extends State {

    override type TBuffer[A] = ParRPBuffer[A]

    override def buffer[A, S <: State](default: A, commitStrategy: (A, A) => A, lock: S#TLock): ParRPBuffer[A] = new ParRPBuffer[A](default, commitStrategy, lock)
    override type TLock = TurnLock
    override def lock(): TurnLock = new TurnLock()
  }
  val parrp: ParRPState = new ParRPState

  class STMState extends State {

    override type TBuffer[A] = STMBuffer[A]
    override def buffer[A, S <: State](default: A, commitStrategy: (A, A) => A, lock: S#TLock): STMBuffer[A] = new STMBuffer[A](default, commitStrategy)
    override type TLock = TurnLock
    override def lock(): TurnLock = new TurnLock()
  }

  val stm: STMState = new STMState
}

final class ParRPBuffer[A](initialValue: A, initialStrategy: (A, A) => A, writeLock: TurnLock) extends Buffer[A] {

  var current: A = initialValue
  private var update: Option[A] = None
  private var owner: Turn[_] = null
  private val commitStrategy: (A, A) => A = initialStrategy

  override def initCurrent(value: A): Unit = synchronized(current = value)


  override def transform(f: (A) => A)(implicit turn: Turn[_]): A = synchronized {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn[_]): Unit = synchronized {
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

  override def base(implicit turn: Turn[_]): A = synchronized(current)

  override def get(implicit turn: Turn[_]): A = synchronized {if (turn eq owner) update.getOrElse(current) else current}

  override def release(implicit turn: Turn[_]): Unit = synchronized {
    update = None
    owner = null
  }

  override def commit(implicit turn: Turn[_]): Unit = synchronized {
    current = commitStrategy(current, get)
    release(turn)
  }
}

final class STMBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] {

  private val current: Ref[A] = Ref(initialValue)
  private val update: Ref[Option[A]] = Ref(None)
  private val commitStrategy: (A, A) => A = initialStrategy

  /** these methods are only used for initialisation and are unsafe to call when the reactive is in use */
  override def initCurrent(value: A): Unit = current.single.set(value)

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
