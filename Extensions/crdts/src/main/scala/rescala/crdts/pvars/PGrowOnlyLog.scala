package rescala.crdts.pvars

import rescala._
import rescala.crdts.statecrdts.sequences.{RGOA, Vertex}

case class PGrowOnlyLog[A](initial: RGOA[A] = RGOA[A](),
                           internalChanges: Evt[RGOA[A]] = Evt[RGOA[A]],
                           externalChanges: Evt[RGOA[A]] = Evt[RGOA[A]]) extends Publishable[RGOA[A]] {

  def append(a: A): Unit = internalChanges.fire(crdtSignal.now.append(Vertex(a)))

  def contains(a: A): Boolean = crdtSignal.now.containsValue(a)

  // allows the log to log events of type a and append them to the log
  def observe(e: Event[A]):Unit = e.observe(a => append(a))
}

object PGrowOnlyLog {
  /**
    * Allows creation of PVertexLogs by passing a set of initial values.
    */
  def apply[A](values: List[A]): PGrowOnlyLog[A] = {
    val init: RGOA[A] = RGOA().fromValue(values)
    new PGrowOnlyLog[A](init)
  }
}
