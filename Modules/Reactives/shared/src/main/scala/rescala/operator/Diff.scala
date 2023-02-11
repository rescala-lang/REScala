package rescala.operator

import rescala.operator.RExceptions.EmptySignalControlThrowable

final class Diff[+A](val from: Pulse[A], val to: Pulse[A]) {

  def _1: A = from.get
  def _2: A = to.get
  def pair: (A, A) = {
    try {
      val right = to.get
      val left  = from.get
      left -> right
    } catch {
      case EmptySignalControlThrowable => throw new NoSuchElementException(s"Can not convert $this to pair")
    }
  }

  override def toString: String = s"Diff($from, $to)"
}

object Diff {
  def apply[A](from: Pulse[A], to: Pulse[A]): Diff[A] = new Diff(from, to)
  def unapply[A](arg: Diff[A]): Option[(A, A)] =
    arg.from match {
      case Pulse.Value(v1) => arg.to match {
          case Pulse.Value(v2) => Some((v1, v2))
          case _               => None
        }
      case _ => None
    }
}
