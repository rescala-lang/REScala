package rescala.stm

import rescala.graph.{Buffer, Committable}
import rescala.propagation.Turn

import scala.concurrent.stm.{InTxn, Ref}

final class STMBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] with Committable {

  private val current: Ref[A] = Ref(initialValue)
  private val update: Ref[Option[A]] = Ref(None)
  private val commitStrategy: (A, A) => A = initialStrategy

  implicit def inTxn(implicit turn: Turn[_]): InTxn = turn match {
    case stmTurn: STMTurn => stmTurn.inTxn
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
