package rescala.graph

import rescala.turns.Turn

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

trait SynchronizationFactory {
  def buffer[A](default: A, commitStrategy: (A, A) => A, lock: ITurnLock): Buffer[A]
  def lock(): ITurnLock
}

object SynchronizationFactory {
  val simple: SynchronizationFactory = new SynchronizationFactory {
    override def buffer[A](default: A, commitStrategy: (A, A) => A, lock: ITurnLock): Buffer[A] = new SimpleBuffer[A](default, commitStrategy)
    override def lock(): ITurnLock = NoLock
  }
}

trait Buffer[A] extends Committable {
  /** these methods are only used for initialisation and are unsafe to call when the reactive is in use */
  def initCurrent(value: A): Unit

  def transform(f: (A) => A)(implicit turn: Turn): A
  def set(value: A)(implicit turn: Turn): Unit
  def base(implicit turn: Turn): A
  def get(implicit turn: Turn): A
  override def release(implicit turn: Turn): Unit
  override def commit(implicit turn: Turn): Unit
}

final class SimpleBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] {

  var current: A = initialValue
  private var update: Option[A] = None
  private var owner: Turn = null
  private val commitStrategy: (A, A) => A = initialStrategy

  override def initCurrent(value: A): Unit = synchronized(current = value)


  override def transform(f: (A) => A)(implicit turn: Turn): A = synchronized {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn): Unit = synchronized {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
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
