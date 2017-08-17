package pvars

import rescala._
import statecrdts.sequences.RGOA

case class PGrowOnlyLog[A](initial: RGOA[A] = RGOA[A](),
                           internalChanges: Evt[RGOA[A]] = Evt[RGOA[A]],
                           externalChanges: Evt[RGOA[A]] = Evt[RGOA[A]]) extends Publishable[RGOA[A]] {

  def append(a: A): Unit = internalChanges(crdtSignal.now.append(Vertex(a)))

  def contains(a: A): Boolean = crdtSignal.now.containsValue(a)
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