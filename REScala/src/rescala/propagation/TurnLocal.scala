package rescala.propagation

import scala.collection.immutable.HashMap

final case class TurnLocal[A](default: A, private val values: Map[Turn, A] = HashMap[Turn, A]()) {
  private def released(implicit turn: Turn): Map[Turn, A] = values - turn
  def withDefault(value: A): TurnLocal[A] = copy(value)
  def transform(f: (A) => A)(implicit turn: Turn): TurnLocal[A] = set(f(get))
  def <<<(f: A => A)(implicit turn: Turn): TurnLocal[A] = transform(f)(turn)
  def set(value: A)(implicit turn: Turn): TurnLocal[A] = copy(values = values.updated(turn, value))
  def <<(value: A)(implicit turn: Turn): TurnLocal[A] = set(value)(turn)
  def get(implicit turn: Turn): A = values.getOrElse(turn, default)
  def combineCommit(combine: (A, A) => A)(implicit turn: Turn): TurnLocal[A] = TurnLocal(combine(default, get), released)
  def release(implicit turn: Turn): TurnLocal[A] = copy(values = released)
  def commit(implicit turn: Turn): TurnLocal[A] = TurnLocal(get, released)
}
