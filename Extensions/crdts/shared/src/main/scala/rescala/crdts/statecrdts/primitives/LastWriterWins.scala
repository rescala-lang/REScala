package rescala.crdts.statecrdts.primitives

import rescala.crdts.statecrdts.StateCRDT

case class LastWriterWins[A](timestamp: Long, payload: A)

object LastWriterWins {
  def apply[A](value: A): LastWriterWins[A] = {
    val timestamp = System.currentTimeMillis()
    LastWriterWins(timestamp, value)
  }

  implicit def LastWriterWinsCRDT[A]: StateCRDT[A, LastWriterWins[A]] = new StateCRDT[A, LastWriterWins[A]] {
    override def value(target: LastWriterWins[A]): A = target.payload

    override def merge(left: LastWriterWins[A], right: LastWriterWins[A]): LastWriterWins[A] =
      if (left.timestamp < right.timestamp) right else left
  }

}
