package rescala.propagation.turns

import scala.collection.immutable.HashMap

final class TurnState[A](@volatile var default: A,
                         @volatile private var values: Map[Turn, A],
                         @volatile var commitStrategy: (A, A) => A,
                         val lock: TurnLock) {

  def cl(implicit turn: Turn): Unit = () //assert(turn.checkLock(lock), "accessed state without holding lock")

  def transform(f: (A) => A)(implicit turn: Turn): Unit = { cl; set(f(get)) }
  def transform(f: PartialFunction[A, A])(implicit turn: Turn): Boolean = {
    cl
    if (f.isDefinedAt(get)) { transform(f: A => A); true }
    else false
  }
  def set(value: A)(implicit turn: Turn): Unit = { cl; values = values.updated(turn, value) }
  def get(implicit turn: Turn): A = { cl; values.getOrElse(turn, default) }
  def release(implicit turn: Turn): Unit = { cl; values -= turn }
  def commit(implicit turn: Turn): Unit = {
    cl
    default = commitStrategy(default, get)
    release
  }
}

object TurnState {
  def apply[A](default: A, commitStrategy: (A, A) => A, lock: TurnLock): TurnState[A] = new TurnState[A](default, HashMap[Turn, A](), commitStrategy, lock)
}
