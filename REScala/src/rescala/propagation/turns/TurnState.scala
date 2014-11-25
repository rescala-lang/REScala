package rescala.propagation.turns

import rescala.propagation.Reactive

import scala.collection.immutable.HashMap

final class TurnState[A](@volatile var default: A,
                         @volatile private var values: Map[Turn, A],
                         @volatile var commitStrategy: (A, A) => A,
                         val owner: Reactive) {

  def cl(implicit turn: Turn): Unit = assert(turn.checkLock(owner.lock), "accessed state without holding lock")

  def transform(f: (A) => A)(implicit turn: Turn): A = {
    cl
    val value = f(get)
    set(value)
    value
  }
  def set(value: A)(implicit turn: Turn): Unit = {
    cl
    values = values.updated(turn, value)
    turn.markForCommit(owner)
  }
  def get(implicit turn: Turn): A = { /*cl;*/ values.getOrElse(turn, default) }
  def getU(implicit turn: Turn): A = { values.getOrElse(turn, default) }
  def release(implicit turn: Turn): Unit = { cl; values -= turn }
  def commit(implicit turn: Turn): Unit = {
    cl
    default = commitStrategy(default, get)
    release
  }
}

object TurnState {
  def apply[A](default: A, commitStrategy: (A, A) => A, lock: Reactive): TurnState[A] = new TurnState[A](default, HashMap[Turn, A](), commitStrategy, lock)
}
