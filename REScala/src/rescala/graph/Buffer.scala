package rescala.graph

import rescala.synchronization.{Key, Prelock, STMSync, TurnLock}
import rescala.turns.Turn

import scala.concurrent.stm.{InTxn, Ref}
import scala.language.implicitConversions

trait Commitable {
  def commit(implicit turn: Turn): Unit
  def release(implicit turn: Turn): Unit
}

object Buffer {
  def apply[A](default: A, commitStrategy: (A, A) => A, writeLock: TurnLock): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, writeLock)

	def commitAsIs[A](base: A, cur: A): A = cur
	def transactionLocal[A](base: A, cur: A) = base
	def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = cur.keep
}

trait Buffer[A] extends Commitable {
  /** these methods are only used for initialisation and are unsafe to call when the reactive is in use */
  def initCurrent(value: A): Unit
  def initStrategy(strategy: (A, A) => A): Unit

  def transform(f: (A) => A)(implicit turn: Turn): A
  def set(value: A)(implicit turn: Turn): Unit
  def base(implicit turn: Turn): A
  def get(implicit turn: Turn): A
  def release(implicit turn: Turn): Unit
  def commit(implicit turn: Turn): Unit
}

final class SimpleBuffer[A](initialValue: A, initialStrategy: (A, A) => A, writeLock: TurnLock) extends Buffer[A] {

  @volatile var current: A = initialValue
  @volatile private var update: Option[A] = None
  @volatile private var owner: Turn = null
  @volatile var commitStrategy: (A, A) => A = initialStrategy

  override def initCurrent(value: A): Unit = synchronized(current = value)
  override def initStrategy(strategy: (A, A) => A): Unit = synchronized(commitStrategy = strategy)


  def transform(f: (A) => A)(implicit turn: Turn): A = synchronized {
    val value = f(get)
    set(value)
    value
  }

  def set(value: A)(implicit turn: Turn): Unit = synchronized {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    turn match {
      case pessimistic: Prelock =>
        val wlo: Option[Key] = Option(writeLock).map(_.getOwner)
        assert(wlo.fold(true)(_ eq pessimistic.key), s"buffer owned by $owner, controlled by $writeLock with owner ${ wlo.get } was written by $turn who locks with ${ pessimistic.key }, by now the owner ist ${ writeLock.getOwner }")
      case _ =>
    }
    update = Some(value)
    owner = turn
    turn.plan(this)
  }

  def base(implicit turn: Turn): A = synchronized(current)

  def get(implicit turn: Turn): A = synchronized { if (turn eq owner) update.getOrElse(current) else current }

  def release(implicit turn: Turn): Unit = synchronized {
    update = None
    owner = null
  }

  def commit(implicit turn: Turn): Unit = synchronized {
    current = commitStrategy(current, get)
    release
  }
}


final class STMBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] {

  private val current: Ref[A] = Ref(initialValue)
  private val update: Ref[Option[A]] = Ref(None)
  private var commitStrategy: (A, A) => A = initialStrategy

  /** these methods are only used for initialisation and are unsafe to call when the reactive is in use */
  override def initCurrent(value: A): Unit = current.single.set(value)
  override def initStrategy(strategy: (A, A) => A): Unit = commitStrategy = strategy

  implicit def inTxn(implicit turn: Turn): InTxn = turn match {
    case stmTurn: STMSync => stmTurn.inTxn
    case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
  }

  def transform(f: (A) => A)(implicit turn: Turn): A = {
    val value = f(get)
    set(value)
    value
  }
  def set(value: A)(implicit turn: Turn): Unit = {
    update.set(Some(value))
    turn.plan(this)
  }
  def base(implicit turn: Turn) = current.get
  def get(implicit turn: Turn): A = update.get.getOrElse(current.get)
  def release(implicit turn: Turn): Unit = {
    update.set(None)
  }
  def commit(implicit turn: Turn): Unit = {
    current.set(commitStrategy(current.get, get))
    release
  }
}
