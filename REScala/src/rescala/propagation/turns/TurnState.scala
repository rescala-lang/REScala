package rescala.propagation.turns

import scala.collection.immutable.HashMap

final class TurnState[A](initialValue: A, initialStrategy: (A, A) => A) extends Commitable {

  @volatile var current: A = initialValue
  @volatile private var values: Map[Turn, A] = HashMap[Turn, A]()
  @volatile var commitStrategy: (A, A) => A = initialStrategy

  def transform(f: (A) => A)(implicit turn: Turn): A = {
    val value = f(get)
    set(value)
    value
  }
  def set(value: A)(implicit turn: Turn): Unit = {
    values = values.updated(turn, value)
    turn.plan(this)
  }
  def base(implicit turn: Turn) = current
  def get(implicit turn: Turn): A = values.getOrElse(turn, current)
  def release(implicit turn: Turn): Unit = values -= turn
  def commit(implicit turn: Turn): Unit = {
    current = commitStrategy(current, get)
    release
  }
}

trait Commitable {
  protected[propagation] def commit(implicit turn: Turn): Unit
  protected[propagation] def release(implicit turn: Turn): Unit
}

object TurnState {
  def apply[A](default: A, commitStrategy: (A, A) => A): TurnState[A] = new TurnState[A](default, commitStrategy)
}
