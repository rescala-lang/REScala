package rescala

import rescala.synchronization.Pessimistic
import rescala.turns.Engine

object DependentUpdate {
  trait Admissions {
    def +=[T]: ((Source[T], T)) => Unit = (doAdmit[T] _).tupled
    def doAdmit[T](source: Source[T], value: T): Unit
  }
  def apply[S, T](signal: Signal[S])(op: (Admissions, S) => T)(implicit engine: Engine): T = {
    engine.startNew { turn =>
      object admissions extends Admissions { override def doAdmit[T0](source: Source[T0], value: T0): Unit = source.admit(value)(turn) }
      turn match {
        case pessimistic: Pessimistic => signal.lock.lock(pessimistic.key)
        case _ => throw new IllegalStateException("currently only supports pessimistic turns")
      }
      val result = op(admissions, signal.get(turn))
      result
    }
  }
}
