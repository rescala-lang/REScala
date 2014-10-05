package rescala.propagation

import scala.collection.immutable.HashMap

final class TurnLocal[A](var default: A, private var values: Map[Turn, A], var commitStrategy: (A, A) => A) {
  def transform(f: (A) => A)(implicit turn: Turn): Unit = set(f(get))
  def transform(f: PartialFunction[A, A])(implicit turn: Turn): Boolean = if (f.isDefinedAt(get)) {set(f(get)); true} else false
  def set(value: A)(implicit turn: Turn): Unit = values = values.updated(turn, value)
  def get(implicit turn: Turn): A = values.getOrElse(turn, default)
  def release(implicit turn: Turn): Unit = values -= turn
  def commit(implicit turn: Turn): Unit = {
    default = commitStrategy(default, get)
    release
  }
}

object TurnLocal {
  def apply[A](default: A, commitStrategy: (A, A) => A): TurnLocal[A] = new TurnLocal[A](default, HashMap[Turn, A](), commitStrategy)
  def apply[A](default: A): TurnLocal[A] = apply(default, (_: A, x: A) => x)
}
