package rescala.graph

import rescala.synchronization.{Prelock, STMSync, TurnLock}
import rescala.turns.Turn

import scala.concurrent.stm.{InTxn, Ref}
import scala.language.implicitConversions

trait Committable {
  def commit(implicit turn: Turn): Unit
  def release(implicit turn: Turn): Unit
}

object Buffer {
  def commitAsIs[A](base: A, cur: A): A = cur
  def transactionLocal[A](base: A, cur: A) = base
  def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = cur.keep
}

trait Buffer[A] extends Committable {
  /** these methods are only used for initialisation and are unsafe to call when the reactive is in use */
  def initCurrent(value: A): Unit
  def initStrategy(strategy: (A, A) => A): Unit

  def transform(f: (A) => A)(implicit turn: Turn): A
  def set(value: A)(implicit turn: Turn): Unit
  def base(implicit turn: Turn): A
  def get(implicit turn: Turn): A
  override def release(implicit turn: Turn): Unit
  override def commit(implicit turn: Turn): Unit
}

final class SimpleBuffer[A](initialValue: A, initialStrategy: (A, A) => A, writeLock: TurnLock) extends Buffer[A] {

  var current: A = initialValue
  private var update: Option[A] = None
  private var owner: Turn = null
  var commitStrategy: (A, A) => A = initialStrategy

  override def initCurrent(value: A): Unit = synchronized(current = value)
  override def initStrategy(strategy: (A, A) => A): Unit = synchronized(commitStrategy = strategy)


  override def transform(f: (A) => A)(implicit turn: Turn): A = synchronized {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn): Unit = synchronized {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    turn match {
      case pessimistic: Prelock =>
        val wlo: Option[Turn] = Option(writeLock).map(_.getOwner)
        assert(wlo.fold(true)(_ eq pessimistic), s"buffer owned by $owner, controlled by $writeLock with owner ${ wlo.get } was written by $turn who locks with ${ pessimistic }, by now the owner ist ${ writeLock.getOwner }")
      case _ =>
    }
    update = Some(value)
    owner = turn
    turn.schedule(this)
  }

  override def base(implicit turn: Turn): A = synchronized(current)

  override def get(implicit turn: Turn): A = synchronized { if (turn eq owner) update.getOrElse(current) else current }

  override def release(implicit turn: Turn): Unit = synchronized {
    update = None
    owner = null
  }

  override def commit(implicit turn: Turn): Unit = synchronized {
    current = commitStrategy(current, get)
    release(turn)
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

  override def transform(f: (A) => A)(implicit turn: Turn): A = {
    val value = f(get)
    set(value)
    value
  }
  override def set(value: A)(implicit turn: Turn): Unit = {
    update.set(Some(value))
    turn.schedule(this)
  }
  override def base(implicit turn: Turn) = current.get
  override def get(implicit turn: Turn): A = update.get.getOrElse(current.get)
  override def release(implicit turn: Turn): Unit = {
    update.set(None)
  }
  override def commit(implicit turn: Turn): Unit = {
    current.set(commitStrategy(current.get, get))
    release
  }
}
