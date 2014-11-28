package rescala.propagation.turns

import rescala.propagation.Reactive

import scala.collection.immutable.HashMap

final class TurnState[A](@volatile var default: A,
                         @volatile private var values: Map[Turn, A],
                         @volatile var commitStrategy: (A, A) => A) extends Commitable {

  def transform(f: (A) => A)(implicit turn: Turn): A = {
    val value = f(get)
    set(value)
    value
  }
  def set(value: A)(implicit turn: Turn): Unit = {
    values = values.updated(turn, value)
    turn.markForCommit(this)
  }
  def get(implicit turn: Turn): A = values.getOrElse(turn, default)
  def release(implicit turn: Turn): Unit = values -= turn
  def commit(implicit turn: Turn): Unit = {
    default = commitStrategy(default, get)
    release
  }
}

trait Commitable {
  def commit(implicit turn: Turn): Unit
  def release(implicit turn: Turn): Unit
}

object TurnState {
  def apply[A](default: A, commitStrategy: (A, A) => A): TurnState[A] = new TurnState[A](default, HashMap[Turn, A](), commitStrategy)
}
